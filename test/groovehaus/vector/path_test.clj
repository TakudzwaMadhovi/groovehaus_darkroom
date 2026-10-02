(ns groovehaus.vector.path-test
  (:require [clojure.test :refer [deftest is testing]]
            [groovehaus.vector.path :as p]))

(defn- near? [a b] (< (Math/abs (- (double a) (double b))) 1e-6))
(defn- ops [segs] (mapv :op segs))

(deftest absolute-and-relative-commands
  (is (= [{:op :move :x 10.0 :y 20.0} {:op :line :x 30.0 :y 20.0} {:op :line :x 30.0 :y 50.0}
          {:op :close}]
         (p/parse-path "M10 20 L30 20 V50 Z")))
  (testing "relative commands accumulate from the current point"
    (is (= [{:op :move :x 10.0 :y 10.0} {:op :line :x 15.0 :y 12.0} {:op :line :x 15.0 :y 20.0}
            {:op :line :x 5.0 :y 20.0}]
           (p/parse-path "m10 10 l5 2 v8 h-10"))))
  (testing "Z returns to subpath start and relative moves start from there"
    (let [segs (p/parse-path "M10 10 L20 10 L20 20 Z m5 5 l1 0")]
      (is (= {:op :move :x 15.0 :y 15.0} (nth segs 4)))
      (is (= {:op :line :x 16.0 :y 15.0} (nth segs 5))))))

(deftest implicit-repetition
  (is (= [{:op :move :x 0.0 :y 0.0} {:op :line :x 10.0 :y 0.0} {:op :line :x 10.0 :y 10.0}]
         (p/parse-path "M0 0 10 0 10 10")) "M followed by extra pairs means L")
  (is (= [:move :line :line] (ops (p/parse-path "m0 0 10 0 0 10"))))
  (is (= [{:op :move :x 0.0 :y 0.0} {:op :line :x 1.0 :y 1.0} {:op :line :x 3.0 :y 3.0}]
         (p/parse-path "M0 0 l1 1 2 2")) "relative repeats are relative to each new point"))

(deftest number-syntax
  (is (= [{:op :move :x 1.5 :y 0.5} {:op :line :x -2.0 :y -3.0}] (p/parse-path "M1.5.5L-2-3e0")))
  (is (= [{:op :move :x 100.0 :y 0.002}] (p/parse-path "M1e2,2E-3")))
  (is (= [] (p/parse-path "")))
  (is (= [] (p/parse-path "  \n "))))

(deftest curves
  (let [[_ c] (p/parse-path "M0 0 C1 2 3 4 5 6")]
    (is (= {:op :cubic :x1 1.0 :y1 2.0 :x2 3.0 :y2 4.0 :x 5.0 :y 6.0} c)))
  (testing "relative cubic"
    (is (= {:op :cubic :x1 11.0 :y1 12.0 :x2 13.0 :y2 14.0 :x 15.0 :y 16.0}
           (second (p/parse-path "M10 10 c1 2 3 4 5 6")))))
  (testing "S reflects the previous cubic control point"
    (let [[_ _ s] (p/parse-path "M0 0 C0 10 10 10 10 0 S20 -10 20 0")]
      (is (= {:op :cubic :x1 10.0 :y1 -10.0 :x2 20.0 :y2 -10.0 :x 20.0 :y 0.0} s))))
  (testing "S without a preceding cubic uses the current point"
    (is (= {:op :cubic :x1 5.0 :y1 5.0 :x2 7.0 :y2 8.0 :x 9.0 :y 10.0}
           (second (p/parse-path "M5 5 S7 8 9 10")))))
  (testing "Q and T"
    (let [[_ q t] (p/parse-path "M0 0 Q5 10 10 0 T20 0")]
      (is (= {:op :quad :x1 5.0 :y1 10.0 :x 10.0 :y 0.0} q))
      (is (= {:op :quad :x1 15.0 :y1 -10.0 :x 20.0 :y 0.0} t) "T reflects the quad control")))
  (testing "T after a non-quad uses the current point as control"
    (is (= {:op :quad :x1 4.0 :y1 4.0 :x 8.0 :y 8.0}
           (second (p/parse-path "M4 4 T8 8"))))))

(deftest arcs
  (is (= {:op :arc :rx 5.0 :ry 3.0 :rotation 30.0 :large? true :sweep? false :x 10.0 :y 0.0}
         (second (p/parse-path "M0 0 A5 3 30 1 0 10 0"))))
  (testing "compact flags without separators"
    (is (= {:op :arc :rx 1.0 :ry 1.0 :rotation 0.0 :large? false :sweep? false :x 0.5 :y 0.5}
           (second (p/parse-path "M0 0a1 1 0 00.5.5")))))
  (testing "relative arc end point"
    (is (= [11.0 12.0] ((juxt :x :y) (second (p/parse-path "M10 10 a2 2 0 0 1 1 2"))))))
  (testing "zero radius degrades to a line, negative radius is abs"
    (is (= {:op :line :x 5.0 :y 5.0} (second (p/parse-path "M0 0 A0 4 0 0 1 5 5"))))
    (is (= [2.0 3.0] ((juxt :rx :ry) (second (p/parse-path "M0 0 A-2 -3 0 0 1 5 5")))))))

(deftest malformed-input
  (doseq [bad ["L10 10" "M0" "M0 0 L1" "M0 0 X5" "M0 0 Z 5 5" "5 5" "M0 0 A1 1 0 2 0 1 1" "M a"]]
    (is (thrown? clojure.lang.ExceptionInfo (p/parse-path bad)) bad))
  (is (= 7 (:index (ex-data (try (p/parse-path "M0 0 L1") (catch Exception e e)))))
      "error index is where the missing number was expected"))

(deftest serialisation-roundtrip
  (let [d "M10 10 L20 10 Q25 15 20 20 C15 25 5 25 0 20 A5 5 0 1 1 10 10 Z"
        segs (p/parse-path d)]
    (is (= segs (p/parse-path (p/segments->d segs))))))

(deftest quad-elevation-is-exact
  (let [q {:op :quad :x1 5.0 :y1 10.0 :x 10.0 :y 0.0}
        c (p/quad->cubic 0.0 0.0 q)
        qpt (fn [t] [(+ (* (- 1 t) (- 1 t) 0) (* 2 (- 1 t) t 5) (* t t 10))
                     (+ (* 2 (- 1 t) t 10))])
        cpt (fn [t] (let [u (- 1 t) b0 (* u u u) b1 (* 3 u u t) b2 (* 3 u t t) b3 (* t t t)]
                      [(+ (* b1 (:x1 c)) (* b2 (:x2 c)) (* b3 (:x c)))
                       (+ (* b1 (:y1 c)) (* b2 (:y2 c)) (* b3 (:y c)))]))]
    (doseq [t [0.0 0.25 0.5 0.9 1.0]]
      (is (every? true? (map near? (qpt t) (cpt t)))))))

(deftest arc-to-cubics
  (testing "semicircle (0,0)->(2,0), sweep=1 passes through (1,-1) in SVG's y-down space"
    (let [segs (p/arc->cubics 0 0 {:rx 1 :ry 1 :rotation 0 :large? false :sweep? true :x 2 :y 0})]
      (is (= 2 (count segs)))
      (is (near? 1 (:x (first segs)))) (is (near? -1 (:y (first segs))))
      (is (near? 2 (:x (last segs)))) (is (near? 0 (:y (last segs))))))
  (testing "sweep=0 goes the other way"
    (let [segs (p/arc->cubics 0 0 {:rx 1 :ry 1 :rotation 0 :large? false :sweep? false :x 2 :y 0})]
      (is (near? 1 (:y (first segs))))))
  (testing "every endpoint lies on the (rotated) ellipse for a large arc"
    (let [seg {:rx 4 :ry 2 :rotation 30 :large? true :sweep? true :x 6 :y 1}
          segs (p/arc->cubics 0 0 seg)
          ;; centre recovered from the endpoints via the same maths; check on-ellipse using a
          ;; second, independent test: both endpoints satisfy the ellipse equation about the centre
          phi (Math/toRadians 30)
          pts (cons [0 0] (map (juxt :x :y) segs))
          ;; solve centre numerically: centre lies where both end points satisfy the equation
          on? (fn [[cx cy] [px py]]
                (let [dx (- px cx) dy (- py cy)
                      u (+ (* (Math/cos phi) dx) (* (Math/sin phi) dy))
                      v (+ (* (- (Math/sin phi)) dx) (* (Math/cos phi) dy))]
                  (+ (/ (* u u) 16.0) (/ (* v v) 4.0))))
          ;; centre = candidate that makes start and end both exactly 1.0
          cands (for [cx (range -10 10 0.05) cy (range -10 10 0.05)
                      :when (and (< (Math/abs (- (on? [cx cy] [0 0]) 1.0)) 0.02)
                                 (< (Math/abs (- (on? [cx cy] [6 1]) 1.0)) 0.02))]
                  [cx cy])]
      (is (> (count segs) 2) "large arc needs more than 180 degrees")
      (is (some (fn [c] (every? #(< (Math/abs (- (on? c %) 1.0)) 0.03) pts)) cands))))
  (testing "too-small radii are scaled up so the arc still reaches the endpoint"
    (let [segs (p/arc->cubics 0 0 {:rx 0.1 :ry 0.1 :rotation 0 :large? false :sweep? true :x 10 :y 0})]
      (is (near? 10 (:x (last segs)))))))

(deftest ->cubics-normalises
  (let [segs (p/->cubics (p/parse-path "M0 0 Q5 5 10 0 A5 5 0 0 1 20 0 Z"))]
    (is (every? #{:move :line :cubic :close} (ops segs)))
    (is (= {:op :close} (last segs)))))
