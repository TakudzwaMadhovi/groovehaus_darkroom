(ns darkroom.imaging.local-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.local :as local]
            [darkroom.imaging.scene :as scene])
  (:import (java.util Random)))

(defn- build [w h f] (scene/image w h (float-array (for [y (range h) x (range w) c (f x y)] c))))
(defn- at [sc x y c] (double (aget ^floats (:data sc) (+ (* 3 (+ (* y (:width sc)) x)) c))))
(defn- flat [w h v] (build w h (fn [_ _] [v v v])))
(defn- layer [m] (merge {:id 1 :visible true :amount 1.0 :invert false :feather 0.0} m))

(deftest nothing-to-do
  (let [img (flat 20 10 0.3)]
    (is (identical? img (local/apply-local img {})))
    (is (identical? img (local/apply-local img {:local []})))
    (is (identical? img (local/apply-local img {:local [(layer {:type :linear :shape {:x0 0 :y0 0 :x1 0 :y1 1} :adj {}})]}))
        "a layer with no adjustments")
    (is (identical? img (local/apply-local img {:local [(layer {:type :linear :shape {:x0 0 :y0 0 :x1 0 :y1 1} :adj {:exposure 0.0}})]})))
    (is (identical? img (local/apply-local img {:local [(layer {:type :linear :visible false :shape {:x0 0 :y0 0 :x1 0 :y1 1} :adj {:exposure 1.0}})]}))
        "hidden")
    (is (identical? img (local/apply-local img {:local [(layer {:type :linear :amount 0.0 :shape {:x0 0 :y0 0 :x1 0 :y1 1} :adj {:exposure 1.0}})]}))
        "zero amount")
    (is (local/local-neutral? {:local []}))
    (is (not (local/local-neutral? {:local [(layer {:type :radial :adj {:exposure 0.5}})]})))))

(deftest linear-gradient-layer
  (let [img (flat 40 100 0.2)
        out (local/apply-local img {:local [(layer {:type :linear :shape {:x0 0.5 :y0 0.0 :x1 0.5 :y1 0.5} :adj {:exposure -1.0}})]})]
    (is (< (at out 20 5 0) 0.12) "darkened at the top (a stop is half the light)")
    (is (< (Math/abs (- 0.1 (at out 20 2 0))) 0.012))
    (is (= (at img 20 80 0) (at out 20 80 0)) "bottom half is bit-for-bit unchanged")
    (let [mid (at out 20 25 0)] (is (< 0.1 mid 0.2) "fades between"))
    (is (apply <= (for [y (range 100)] (at out 20 y 0))) "brightness rises monotonically down the gradient")))

(deftest radial-layer
  (let [img (flat 100 100 0.1)
        out (local/apply-local img {:local [(layer {:type :radial :shape {:cx 0.5 :cy 0.5 :rx 0.3 :ry 0.3 :feather 0.5} :adj {:exposure 1.0}})]})]
    (is (< (Math/abs (- 0.2 (at out 50 50 0))) 0.01) "doubled in the centre")
    (is (= (at img 2 2 0) (at out 2 2 0)) "corners untouched")))

(deftest brush-layer
  (let [img (flat 100 100 0.1)
        out (local/apply-local img {:local [(layer {:type :brush :shape {:strokes [{:points [[0.2 0.5] [0.8 0.5]] :radius 0.06 :feather 0.3}]} :adj {:exposure 1.0}})]})]
    (is (> (at out 50 50 0) 0.19)) (is (= (at img 50 5 0) (at out 50 5 0)))))

(deftest amount-scales-the-effect
  (let [img (flat 30 30 0.2)
        mk (fn [a] (local/apply-local img {:local [(layer {:type :range :amount a :adj {:exposure 1.0}})]}))
        full (at (mk 1.0) 5 5 0) half (at (mk 0.5) 5 5 0)]
    (is (< (Math/abs (- 0.4 full)) 0.01))
    (is (< (Math/abs (- 0.3 half)) 0.01) "halfway between untouched and fully adjusted")))

(deftest invert
  (let [img (flat 40 100 0.2)
        mk (fn [inv] (local/apply-local img {:local [(layer {:type :linear :invert inv :shape {:x0 0.5 :y0 0.0 :x1 0.5 :y1 0.5} :adj {:exposure -1.0}})]}))
        a (mk false) b (mk true)]
    (is (< (at a 20 2 0) 0.12)) (is (< (Math/abs (- 0.2 (at b 20 2 0))) 0.002)) (is (< (at b 20 80 0) 0.2))))

(deftest range-limits
  (testing "luminance range: only the bright half is touched"
    (let [img (build 100 10 (fn [x _] (let [v (if (< x 50) 0.05 0.6)] [v v v])))
          out (local/apply-local img {:local [(layer {:type :range :range {:luma {:lo 0.5 :hi 1.0 :smooth 0.05}} :adj {:exposure -1.0}})]})]
      (is (= (at img 10 5 0) (at out 10 5 0)) "dark side untouched")
      (is (< (at out 80 5 0) 0.35))))
  (testing "colour range: only red is desaturated"
    (let [img (build 60 10 (fn [x _] (cond (< x 20) [0.7 0.1 0.1] (< x 40) [0.1 0.6 0.1] :else [0.1 0.1 0.7])))
          out (local/apply-local img {:local [(layer {:type :range :range {:color {:hue 0 :range 25}} :adj {:saturation -1.0}})]})
          spread (fn [sc x] (- (apply max (map #(at sc x 5 %) [0 1 2])) (apply min (map #(at sc x 5 %) [0 1 2]))))]
      (is (< (spread out 10) (* 0.3 (spread img 10))) "red drained")
      (is (= (spread img 30) (spread out 30)) "green untouched")))
  (testing "a gradient limited by a range multiplies the two"
    (let [img (build 100 100 (fn [_ y] (let [v (if (< y 50) 0.6 0.05)] [v v v])))
          out (local/apply-local img {:local [(layer {:type :linear :shape {:x0 0.5 :y0 0.0 :x1 0.5 :y1 1.0}
                                                      :range {:luma {:lo 0.5 :hi 1.0 :smooth 0.05}} :adj {:exposure -1.0}})]})]
      (is (< (at out 50 5 0) 0.5) "bright and in the gradient")
      (is (= (at img 50 90 0) (at out 50 90 0)) "dark: out of the range"))))

(deftest detail-adjustments-stay-in-the-mask
  (let [r (Random. 5)
        img (build 100 100 (fn [_ _] (let [v (+ 0.25 (* 0.03 (.nextGaussian r)))] [v v v])))
        out (local/apply-local img {:local [(layer {:type :radial :shape {:cx 0.3 :cy 0.5 :rx 0.2 :ry 0.4 :feather 0.1} :adj {:texture 1.0}})]})
        variance (fn [sc x0 x1] (let [vs (for [y (range 20 80) x (range x0 x1)] (at sc x y 0)) m (/ (reduce + vs) (count vs))]
                             (/ (reduce + (map #(* (- % m) (- % m)) vs)) (count vs))))]
    (is (> (variance out 20 40) (* 1.5 (variance img 20 40))) "texture raised inside")
    (is (< (Math/abs (- (variance out 80 98) (variance img 80 98))) (* 0.05 (variance img 80 98))) "and not far outside")))

(deftest subject-and-sky-layers
  (let [r (Random. 4) w 160 h 120
        subject-img (build w h (fn [x y] (let [d (Math/hypot (- x 80.0) (- y 60.0)) n (fn [] (* 0.02 (.nextGaussian r)))]
                                           (if (< d 28.0) [(+ 0.5 (n)) (+ 0.25 (n)) (+ 0.1 (n))] [(+ 0.05 (n)) (+ 0.15 (n)) (+ 0.35 (n))]))))
        out (local/apply-local subject-img {:local [(layer {:type :subject :shape {:rect [0.25 0.15 0.5 0.7]} :adj {:exposure 1.0}})]})]
    (is (> (at out 80 60 0) (* 1.7 (at subject-img 80 60 0))) "the subject is brightened")
    (is (< (Math/abs (- (at out 5 5 2) (at subject-img 5 5 2))) 0.03) "the background is not"))
  (let [img (build 100 100 (fn [x y] (if (< y 50) [0.3 0.5 0.9] [0.1 0.25 0.05])))
        out (local/apply-local img {:local [(layer {:type :sky :shape {} :adj {:exposure -1.0}})]})]
    (is (< (at out 50 10 2) (* 0.65 (at img 50 10 2))) "sky darkened")
    (is (< (Math/abs (- (at out 50 90 1) (at img 50 90 1))) 0.01) "ground not")))

(deftest layers-apply-in-order-and-compound
  (let [img (flat 40 40 0.1)
        L (fn [id a] (layer {:id id :type :range :adj {:exposure a}}))
        out (local/apply-local img {:local [(L 1 1.0) (L 2 1.0)]})]
    (is (< (Math/abs (- 0.4 (at out 5 5 0))) 0.02) "two stops")))

(deftest feather-softens-a-mask-edge
  (let [img (flat 200 20 0.2)
        mk (fn [f] (local/apply-local img {:local [(layer {:type :brush :feather f :shape {:strokes [{:points [[0.5 0.5]] :radius 0.15 :feather 0.0}]} :adj {:exposure 1.0}})]}))
        width-of (fn [sc] (count (filter #(< 0.205 (at sc % 10 0) 0.395) (range 200))))]
    (is (> (width-of (mk 1.0)) (+ 3 (width-of (mk 0.0)))))))

(deftest masks-are-cached-for-scene-independent-layers
  (let [img (flat 50 50 0.2)
        l (layer {:type :brush :shape {:strokes [{:points [[0.5 0.5]] :radius 0.1}]} :adj {:exposure 1.0}})
        a (local/layer-mask img l) b (local/layer-mask img l)]
    (is (identical? a b))
    (is (not (identical? a (local/layer-mask img (assoc-in l [:shape :strokes 0 :radius] 0.2)))))))

(deftest layer-mask-shape
  (let [img (flat 30 20 0.3) m (local/layer-mask img (layer {:type :range}))]
    (is (= 600 (alength m))) (is (every? #(== 1.0 %) m))))

;; --- helpers for the UI ---------------------------------------------------------------

(deftest new-layers
  (doseq [t [:linear :radial :brush :range :subject :sky]]
    (let [l (local/new-layer t 7)]
      (is (= 7 (:id l))) (is (= t (:type l))) (is (:visible l)) (is (= 1.0 (:amount l))) (is (= {} (:adj l)))
      (is (contains? local/type-labels t))
      (is (not (local/layer-active? l)) "no adjustments yet, so it does nothing")))
  (testing "every kind of fresh layer is a valid mask"
    (let [img (flat 40 30 0.3)]
      (doseq [t [:linear :radial :brush :range :sky]]
        (let [m (local/layer-mask img (local/new-layer t 1))]
          (is (= 1200 (alength m)) (str t)) (is (every? #(<= 0.0 % 1.0) m) (str t))))))
  (is (= 1 (local/next-id [])))
  (is (= 6 (local/next-id [{:id 2} {:id 5}]))))

(deftest layer-titles
  (is (= "LINEAR GRADIENT" (local/layer-title (local/new-layer :linear 1))))
  (is (re-find #"BRUSH · EXPOSURE \+0.50" (local/layer-title (assoc (local/new-layer :brush 1) :adj {:exposure 0.5 :contrast 0.0})))))

(deftest painting-a-mask
  (let [src {:width 2 :height 1 :pixels (int-array [(unchecked-int 0xFF808080) (unchecked-int 0xFF808080)])}
        out (local/paint-mask src (float-array [0.0 1.0]))
        ch (fn [p s] (bit-and (unsigned-bit-shift-right p s) 0xFF))]
    (is (= (aget ^ints (:pixels src) 0) (aget ^ints (:pixels out) 0)) "unmasked pixel unchanged")
    (is (> (ch (aget ^ints (:pixels out) 1) 16) 180)) (is (< (ch (aget ^ints (:pixels out) 1) 8) 90))
    (is (= 255 (ch (aget ^ints (:pixels out) 1) 24)))))
