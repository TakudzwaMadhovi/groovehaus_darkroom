(ns darkroom.imaging.crop-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.crop :as crop]
            [darkroom.imaging.geometry :as geometry]))

(defn- near? [a b] (every? true? (map #(< (Math/abs (- (double %1) (double %2))) 1e-9) a b)))
(defn- inside? [[x y w h]] (and (>= x -1e-12) (>= y -1e-12) (<= (+ x w) (+ 1.0 1e-12)) (<= (+ y h) (+ 1.0 1e-12))))
(def ^:private r0 [0.2 0.3 0.5 0.4])

(deftest lock-ratios
  (is (nil? (crop/lock-ratio nil 1.5)))
  (is (nil? (crop/lock-ratio :free 1.5)))
  (is (= 1.0 (crop/lock-ratio :orig 1.5)) "the original shape is a square in fraction space")
  (is (< (Math/abs (- 1.0 (crop/lock-ratio 1.5 1.5))) 1e-12) "3:2 on a 3:2 frame = the whole frame")
  (is (< (Math/abs (- 0.75 (crop/lock-ratio 1.0 (/ 4.0 3.0)))) 1e-12) "a square on a 4:3 frame is 3/4 as wide as it is tall (as fractions)"))

(deftest moving
  (is (near? [0.25 0.35 0.5 0.4] (crop/move r0 0.05 0.05)))
  (testing "stops at the edges without shrinking"
    (is (near? [0.5 0.6 0.5 0.4] (crop/move r0 5.0 5.0)))
    (is (near? [0.0 0.0 0.5 0.4] (crop/move r0 -5.0 -5.0)))))

(deftest full-frame-detection
  (is (crop/full? [0.0 0.0 1.0 1.0]))
  (is (crop/full? [0.0005 0.0 0.9995 1.0]))
  (is (not (crop/full? [0.0 0.0 0.9 1.0])))
  (is (not (crop/full? r0))))

(deftest drawing-a-new-crop
  (is (near? [0.1 0.2 0.4 0.3] (crop/from-corners 0.1 0.2 0.5 0.5 nil)))
  (testing "dragging up and left of the anchor"
    (is (near? [0.1 0.2 0.4 0.3] (crop/from-corners 0.5 0.5 0.1 0.2 nil))))
  (testing "clamped to the frame"
    (is (inside? (crop/from-corners 0.8 0.8 3.0 3.0 nil)))
    (is (near? [0.8 0.8 0.2 0.2] (crop/from-corners 0.8 0.8 3.0 3.0 nil))))
  (testing "never smaller than the minimum"
    (let [[_ _ w h] (crop/from-corners 0.5 0.5 0.5 0.5 nil)]
      (is (>= w crop/min-size)) (is (>= h crop/min-size))))
  (testing "a lock holds the ratio whatever the drag"
    (doseq [lock [0.5 1.0 1.6] [mx my] [[0.9 0.6] [0.2 0.9] [0.95 0.1] [0.3 0.3]]]
      (let [[x y w h :as r] (crop/from-corners 0.4 0.4 mx my lock)]
        (is (inside? r)) (is (< (Math/abs (- lock (/ w h))) 1e-9) (str lock [mx my]))))))

(deftest corner-handles
  (testing "the opposite corner stays put"
    (let [[x y w h] (crop/resize r0 :nw 0.1 0.1 nil)]
      (is (near? [(+ 0.2 0.5) (+ 0.3 0.4)] [(+ x w) (+ y h)])) (is (near? [0.1 0.1] [x y])))
    (let [[x y w h] (crop/resize r0 :se 0.9 0.9 nil)]
      (is (near? [0.2 0.3] [x y])) (is (near? [0.9 0.9] [(+ x w) (+ y h)])))
    (let [[x y w h] (crop/resize r0 :ne 0.9 0.1 nil)]
      (is (near? [0.2 0.7] [x (+ y h)])) (is (near? [0.9 0.1] [(+ x w) y])))
    (let [[x y w h] (crop/resize r0 :sw 0.05 0.95 nil)]
      (is (near? [0.7 0.3] [(+ x w) y])) (is (near? [0.05 0.95] [x (+ y h)]))))
  (testing "dragging past the opposite corner flips the rectangle rather than inverting it"
    (let [[x y w h :as r] (crop/resize r0 :se 0.1 0.1 nil)]
      (is (inside? r)) (is (pos? w)) (is (pos? h)) (is (near? [0.1 0.1] [x y])))))

(deftest edge-handles
  (testing "free edges move one side only"
    (is (near? [0.2 0.1 0.5 0.6] (crop/resize r0 :n 0.0 0.1 nil)))
    (is (near? [0.2 0.3 0.5 0.6] (crop/resize r0 :s 0.0 0.9 nil)))
    (is (near? [0.05 0.3 0.65 0.4] (crop/resize r0 :w 0.05 0.0 nil)))
    (is (near? [0.2 0.3 0.7 0.4] (crop/resize r0 :e 0.9 0.0 nil))))
  (testing "never below the minimum"
    (let [[_ _ _ h] (crop/resize r0 :n 0.0 0.69999 nil)] (is (>= h crop/min-size)))
    (let [[_ _ w _] (crop/resize r0 :e 0.2 0.0 nil)] (is (>= w crop/min-size)))))

(deftest locked-resizing-keeps-the-ratio-and-stays-in-the-frame
  (doseq [lock [0.6 1.0 1.8]
          handle [:nw :n :ne :e :se :s :sw :w]
          [px py] [[0.05 0.05] [0.95 0.95] [0.5 0.1] [0.1 0.5] [0.9 0.3] [0.3 0.9] [0.0 1.0]]]
    (let [start (crop/from-corners 0.3 0.3 0.7 0.7 lock)
          [x y w h :as r] (crop/resize start handle px py lock)]
      (is (inside? r) (str handle [px py] lock r))
      (is (< (Math/abs (- lock (/ w h))) 1e-9) (str handle [px py] lock r)))))

(deftest unlocked-resizing-stays-in-the-frame
  (doseq [handle [:nw :n :ne :e :se :s :sw :w] [px py] [[-1.0 -1.0] [2.0 2.0] [0.5 0.5] [0.0 1.0]]]
    (is (inside? (crop/resize r0 handle px py nil)) (str handle [px py]))))

(deftest centred-rectangles
  (testing "a wide ratio on a wide frame"
    (let [fa (/ 3.0 2.0)]
      (let [[x y w h] (crop/centered (/ 16.0 9.0) fa)]
        (is (< (Math/abs (- (/ 16.0 9.0) (/ (* w fa) h))) 1e-9) "the pixel ratio is right")
        (is (= 1.0 w)) (is (< (+ y h y) 1.0000001)))))
  (testing "a tall ratio on a wide frame"
    (let [fa 1.5 [x y w h] (crop/centered 0.8 fa)]
      (is (< (Math/abs (- 0.8 (/ (* w fa) h))) 1e-9)) (is (= 1.0 h)) (is (< (Math/abs (- x (/ (- 1.0 w) 2.0))) 1e-12))))
  (testing "agrees with the engine's own centred crop"
    (let [[x y w h] (crop/centered 0.8 (/ 160.0 90.0))
          [ex ey ew eh] (geometry/crop-fractions {:aspect "4:5"} 160 90)]
      (is (near? [ex ey ew eh] [x y w h])))))

(deftest hit-testing
  (let [tol-x 0.02 tol-y 0.02 at (fn [px py] (crop/handle-at r0 px py tol-x tol-y))]
    (is (= :nw (at 0.2 0.3))) (is (= :ne (at 0.7 0.3))) (is (= :se (at 0.7 0.7))) (is (= :sw (at 0.2 0.7)))
    (is (= :n (at 0.45 0.3))) (is (= :s (at 0.45 0.7))) (is (= :w (at 0.2 0.5))) (is (= :e (at 0.7 0.5)))
    (is (= :move (at 0.45 0.5)))
    (is (nil? (at 0.05 0.05)))
    (is (= :nw (at 0.19 0.31)) "a little outside still counts")))
