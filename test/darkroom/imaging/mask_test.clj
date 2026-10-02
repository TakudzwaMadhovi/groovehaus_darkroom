(ns darkroom.imaging.mask-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.mask :as mask]
            [darkroom.imaging.scene :as scene])
  (:import (java.util Random)))

(defn- at [^floats m w x y] (double (aget m (+ (* y w) x))))
(defn- build [w h f] (scene/image w h (float-array (for [y (range h) x (range w) c (f x y)] c))))
(defn- near? [expected actual tol] (< (Math/abs (- (double expected) (double actual))) tol))

;; --- gradients -------------------------------------------------------------------

(deftest linear-gradient
  (let [w 100 h 50 m (mask/linear w h {:x0 0.25 :y0 0.5 :x1 0.75 :y1 0.5})]
    (is (= (* w h) (alength m)))
    (is (near? 1.0 (at m w 5 10) 1e-6) "full effect behind the start")
    (is (near? 0.0 (at m w 95 10) 1e-6) "none past the end")
    (is (near? 0.5 (at m w 50 25) 0.02) "half way")
    (is (apply >= (for [x (range w)] (at m w x 25))) "monotonic along the gradient")
    (is (every? #(near? (at m w 40 0) (at m w 40 %) 1e-6) (range h)) "constant across it"))
  (testing "reversing the points flips it"
    (let [a (mask/linear 100 20 {:x0 0.2 :y0 0.5 :x1 0.8 :y1 0.5})
          b (mask/linear 100 20 {:x0 0.8 :y0 0.5 :x1 0.2 :y1 0.5})]
      (is (near? 1.0 (+ (at a 100 30 5) (at b 100 30 5)) 1e-6))))
  (testing "a diagonal gradient is constant along its perpendicular"
    (let [m (mask/linear 80 80 {:x0 0.2 :y0 0.2 :x1 0.8 :y1 0.8})]
      (is (near? (at m 80 30 50) (at m 80 50 30) 1e-6))))
  (testing "coincident points give an all-ones mask rather than NaN"
    (is (every? #(== 1.0 %) (mask/linear 10 10 {:x0 0.5 :y0 0.5 :x1 0.5 :y1 0.5})))))

(deftest radial-gradient
  (let [w 100 h 100 m (mask/radial w h {:cx 0.5 :cy 0.5 :rx 0.25 :ry 0.25 :feather 0.5})]
    (is (near? 1.0 (at m w 50 50) 1e-6))
    (is (near? 0.0 (at m w 2 2) 1e-6))
    (is (near? 1.0 (at m w 60 50) 1e-6) "inside the un-feathered core")
    (let [mid (at m w 70 50)] (is (< 0.05 mid 0.95) "feather zone"))
    (is (apply >= (for [x (range 50 100)] (at m w x 50))) "falls off outward"))
  (testing "a small feather is nearly a hard edge"
    (let [m (mask/radial 200 200 {:cx 0.5 :cy 0.5 :rx 0.25 :ry 0.25 :feather 0.01})]
      (is (> (at m 200 145 100) 0.95)) (is (< (at m 200 155 100) 0.05))))
  (testing "radii are fractions of width and height separately"
    (let [m (mask/radial 200 100 {:cx 0.5 :cy 0.5 :rx 0.4 :ry 0.1 :feather 0.05})]
      (is (> (at m 200 150 50) 0.9) "wide")
      (is (< (at m 200 100 70) 0.1) "but short"))))

;; --- brush ------------------------------------------------------------------------

(deftest brush-strokes
  (let [w 100 h 100]
    (testing "no strokes, no mask"
      (is (every? zero? (mask/brush w h []))))
    (testing "one dab"
      (let [m (mask/brush w h [{:points [[0.5 0.5]] :radius 0.1 :feather 0.5 :flow 1.0}])]
        (is (near? 1.0 (at m w 50 50) 1e-6)) (is (near? 0.0 (at m w 70 50) 1e-6))
        (is (< 0.05 (at m w 57 50) 1.0) "soft edge")))
    (testing "flow sets the opacity, and dabs accumulate"
      (let [one (mask/brush w h [{:points [[0.5 0.5]] :radius 0.1 :flow 0.5}])
            two (mask/brush w h [{:points [[0.5 0.5]] :radius 0.1 :flow 0.5} {:points [[0.5 0.5]] :radius 0.1 :flow 0.5}])]
        (is (near? 0.5 (at one w 50 50) 1e-6)) (is (near? 0.75 (at two w 50 50) 1e-6))))
    (testing "a stroke is continuous along its path"
      (let [m (mask/brush w h [{:points [[0.1 0.5] [0.9 0.5]] :radius 0.05 :feather 0.3}])]
        (is (every? #(> (at m w % 50) 0.99) (range 10 90)))
        (is (< (at m w 50 60) 0.01))))
    (testing "an erase stroke takes paint away"
      (let [m (mask/brush w h [{:points [[0.2 0.5] [0.8 0.5]] :radius 0.08}
                               {:points [[0.5 0.5]] :radius 0.04 :erase? true}])]
        (is (near? 0.0 (at m w 50 50) 1e-6)) (is (near? 1.0 (at m w 25 50) 1e-6))))
    (testing "values stay in range and edges of the image are safe"
      (let [m (mask/brush w h [{:points [[-0.1 -0.1] [1.2 1.2]] :radius 0.2 :feather 1.0 :flow 1.0}])]
        (is (every? #(<= 0.0 % 1.0) m))))))

;; --- range masks --------------------------------------------------------------------

(deftest luminance-range
  (let [w 200 img (build w 4 (fn [x _] (let [v (scene/srgb-decode-extended (/ x 199.0))] [v v v])))]
    (let [m (mask/luminance img {:lo 0.4 :hi 0.6 :smooth 0.05})]
      (is (near? 0.0 (at m w 20 1) 0.01)) (is (near? 1.0 (at m w 100 1) 0.01)) (is (near? 0.0 (at m w 180 1) 0.01)))
    (testing "open ends"
      (let [dark (mask/luminance img {:lo 0.0 :hi 0.3 :smooth 0.05})]
        (is (near? 1.0 (at dark w 0 1) 1e-6)) (is (near? 0.0 (at dark w 150 1) 0.01)))
      (let [bright (mask/luminance img {:lo 0.7 :hi 1.0 :smooth 0.05})]
        (is (near? 1.0 (at bright w 199 1) 1e-6)) (is (near? 0.0 (at bright w 40 1) 0.01))))
    (testing "values stay in range"
      (is (every? #(<= 0.0 % 1.0) (mask/luminance img {:lo 0.3 :hi 0.8}))))))

(deftest hue-range-mask
  (let [w 30 img (build w 10 (fn [x _] (cond (< x 10) [0.8 0.1 0.1] (< x 20) [0.1 0.7 0.1] :else [0.4 0.4 0.4])))
        red (mask/hue-range img {:hue 0 :range 25})]
    (is (> (at red w 5 5) 0.95) "red") (is (< (at red w 15 5) 0.05) "not green") (is (< (at red w 25 5) 0.05) "never grey"))
  (testing "hue distance wraps around 360"
    (let [img (build 2 1 (fn [x _] (if (zero? x) [0.8 0.1 0.12] [0.8 0.12 0.1])))
          m (mask/hue-range img {:hue 355 :range 25})]
      (is (and (> (at m 2 0 0) 0.8) (> (at m 2 1 0) 0.8))))))

;; --- subject and sky -----------------------------------------------------------------

(deftest subject-selection
  (let [r (Random. 4) w 160 h 120
        img (build w h (fn [x y] (let [d (Math/hypot (- x 80.0) (- y 60.0))
                                       n (fn [] (* 0.02 (.nextGaussian r)))]
                                   (if (< d 28.0)
                                     [(+ 0.8 (n)) (+ 0.35 (n)) (+ 0.1 (n))]
                                     [(+ 0.05 (n)) (+ 0.15 (n)) (+ 0.35 (n))]))))
        m (mask/subject img {:rect [0.25 0.15 0.5 0.7]})]
    (is (= (* w h) (alength m)))
    (is (> (at m w 80 60) 0.8) "the subject is selected")
    (is (> (at m w 70 50) 0.8))
    (is (< (at m w 5 5) 0.2) "the background is not")
    (is (< (at m w 150 110) 0.2))
    (is (< (at m w 80 8) 0.2) "even inside the rectangle's margin")
    (is (every? #(<= -1e-6 % 1.000001) m))))

(deftest sky-selection
  (let [r (Random. 8) w 160 h 120
        img (build w h (fn [x y]
                         (cond
                           ;; a dark "tree" in the sky
                           (and (< 30 y 55) (< 100 x 130)) [0.02 0.04 0.02]
                           ;; sky: bluish, brighter toward the horizon
                           (< y 60) (let [t (/ y 60.0) v (+ 0.5 (* 0.35 t))] [(* 0.55 v) (* 0.75 v) v])
                           ;; ground: textured green/brown
                           :else (let [n (* 0.15 (.nextDouble r))] [(+ 0.1 n) (+ 0.2 (* 0.5 n)) (+ 0.05 n)]))))
        m (mask/sky img {})]
    (is (> (at m w 20 10) 0.9) "open sky")
    (is (> (at m w 70 50) 0.8) "toward the horizon")
    (is (< (at m w 115 42) 0.35) "the tree is not sky")
    (is (< (at m w 80 100) 0.1) "the ground is not")
    (is (< (at m w 20 115) 0.1))))

(deftest sky-needs-a-connection-to-the-top
  ;; a bright blue patch on the ground (a pool, a blue car) is not sky
  (let [img (build 100 100 (fn [x y] (cond (< y 30) [0.3 0.45 0.8]
                                           (and (< 40 x 70) (< 60 y 90)) [0.3 0.45 0.8]
                                           :else [0.1 0.2 0.05])))
        m (mask/sky img {})]
    (is (> (at m 100 20 10) 0.9)) (is (< (at m 100 55 75) 0.1))))
