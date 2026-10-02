(ns darkroom.imaging.heal-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.heal :as heal]
            [darkroom.imaging.scene :as scene])
  (:import (java.util Random)))

(defn- build [w h f] (scene/image w h (float-array (for [y (range h) x (range w) c (f x y)] c))))
(defn- at [sc x y c] (double (aget ^floats (:data sc) (+ (* 3 (+ (* y (:width sc)) x)) c))))
(defn- dot? [x y cx cy r] (< (Math/hypot (- x cx) (- y cy)) r))

(defn- background
  "A textured mid-tone (0.3 +- 0.02) with a bright blob at (bx, by)."
  [w h blobs]
  (let [r (Random. 3)]
    (build w h (fn [x y] (let [n (* 0.02 (.nextGaussian r)) v (+ 0.3 n)]
                           (if (some (fn [[bx by br]] (dot? x y bx by br)) blobs) [0.9 0.9 0.9] [v v v]))))))

(deftest spots-neutral
  (let [img (background 20 20 [])]
    (is (identical? img (heal/apply-spots img {})))
    (is (identical? img (heal/apply-spots img {:spots []})))))

(deftest healing-removes-a-blemish
  (let [img (background 120 90 [[60 45 4]])
        out (heal/apply-spots img {:spots [{:x 0.5 :y 0.5 :r 0.07 :mode :heal}]})]
    (is (> (at img 60 45 0) 0.85) "the blemish is there")
    (is (< (Math/abs (- 0.3 (at out 60 45 0))) 0.06) "healed to the surrounding tone")
    (is (< (Math/abs (- 0.3 (at out 58 44 1))) 0.06))
    (testing "far away pixels are untouched"
      (is (= (at img 5 5 0) (at out 5 5 0))) (is (= (at img 110 80 2) (at out 110 80 2))))
    (testing "the input is not modified"
      (is (> (at img 60 45 0) 0.85)))
    (is (= [120 90] [(:width out) (:height out)]))))

(deftest cloning-copies-the-chosen-patch
  (let [img (build 100 60 (fn [x _] (if (< x 50) [0.2 0.2 0.2] [0.7 0.7 0.7])))
        ;; clone a patch from the bright right half onto the dark left half
        out (heal/apply-spots img {:spots [{:x 0.2 :y 0.5 :r 0.06 :mode :clone :sx 0.7 :sy 0.5 :feather 0.2}]})]
    (is (> (at out 20 30 0) 0.65) "the centre now matches the source")
    (is (< (Math/abs (- 0.2 (at out 5 30 0))) 1e-6) "outside the spot nothing changed")
    (testing "the edge is feathered, not hard"
      (let [r (* 0.06 100) edge (at out (+ 20 (int (* 0.9 r))) 30 0)]
        (is (< 0.2 edge 0.7))))))

(deftest opacity-blends-the-clone
  (let [img (build 100 60 (fn [x _] (if (< x 50) [0.2 0.2 0.2] [0.8 0.8 0.8])))
        out (heal/apply-spots img {:spots [{:x 0.2 :y 0.5 :r 0.06 :mode :clone :sx 0.7 :sy 0.5 :opacity 0.5}]})]
    (is (< (Math/abs (- 0.5 (at out 20 30 0))) 0.03))))

(deftest auto-source-avoids-other-blemishes-and-overlap
  ;; a second blemish sits 2.5 radii to the right: the source must not be there
  (let [r 8.0
        img (background 160 100 [[60 50 4] [80 50 4]])
        [sx sy] (heal/auto-source img 60.0 50.0 r)]
    (is (>= (Math/hypot (- sx 60.0) (- sy 50.0)) (* 2.0 r)) "clear of the spot itself")
    (is (> (Math/hypot (- sx 80.0) (- sy 50.0)) (* 1.5 r)) "and of its neighbour")
    (testing "result lies inside the picture with room for the patch"
      (is (< r sx (- 160 r))) (is (< r sy (- 100 r))))))

(deftest auto-source-prefers-a-matching-surround
  ;; left of the spot is a dark region, right of it matches: the source comes from the right
  (let [img (build 200 80 (fn [x _] (if (< x 70) [0.05 0.05 0.05] [0.5 0.5 0.5])))
        r 6.0
        [sx _] (heal/auto-source img 100.0 40.0 r)]
    (is (> sx 100.0))))

(deftest spots-near-the-edge-still-work
  (let [img (background 100 80 [[3 3 3]])
        out (heal/apply-spots img {:spots [{:x 0.03 :y 0.04 :r 0.05 :mode :heal}]})]
    (is (< (at out 3 3 0) 0.5) "healed (via the clone fallback where the patch cannot fit)")
    (is (every? #(Double/isFinite (double %)) (:data out)))))

(deftest several-spots-apply-in-order
  (let [img (background 150 100 [[40 30 4] [100 70 4]])
        out (heal/apply-spots img {:spots [{:x (/ 40.0 150) :y 0.3 :r 0.06 :mode :heal} {:x (/ 100.0 150) :y 0.7 :r 0.06 :mode :heal}]})]
    (is (< (at out 40 30 0) 0.5)) (is (< (at out 100 70 0) 0.5))))
