(ns darkroom.imaging.scene-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.core :as core]
            [darkroom.imaging.scene :as scene]))

(defn- px [r g b] (unchecked-int (bit-or 0xFF000000 (bit-shift-left r 16) (bit-shift-left g 8) b)))
(defn- ch [p s] (bit-and (unsigned-bit-shift-right p s) 0xFF))
(defn- rgb [p] [(ch p 16) (ch p 8) (ch p 0)])
(defn- grey-scene
  "1-row scene image of neutral greys with the given linear values."
  [& vs]
  (scene/image (count vs) 1 (float-array (mapcat (fn [v] [v v v]) vs))))
(defn- argb-of [sc & [space]] (vec (:pixels (if space (scene/->argb sc space) (scene/->argb sc)))))

(deftest argb-round-trip-is-exact
  (let [all (for [v (range 256)] (px v v v))
        col (for [r (range 0 256 15) g (range 0 256 17) b (range 0 256 19)] (px r g b))
        img (core/image (+ 256 (count col)) 1 (int-array (concat all col)))]
    (is (= (vec (:pixels img)) (argb-of (scene/from-argb img)))
        "8-bit sRGB -> float working space -> 8-bit sRGB loses nothing")))

(deftest from-argb-linearises
  (let [sc (scene/from-argb (core/image 3 1 (int-array [(px 0 0 0) (px 255 255 255) (px 128 128 128)])))
        d  (:data sc)]
    (is (= [3 1] [(:width sc) (:height sc)]))
    (is (= 9 (alength ^floats d)))
    (is (every? #(< (Math/abs (double %)) 1e-6) (take 3 d)))
    (is (every? #(< (Math/abs (- 1.0 (double %))) 1e-4) (take 3 (drop 3 d))) "white is 1.0 in every channel")
    (is (every? #(< (Math/abs (- 0.2159 (double %))) 1e-3) (drop 6 d)) "sRGB 128 = 21.6% linear light")))

(deftest from-linear16-scales
  (let [sc (scene/from-linear16 {:width 2 :height 1 :data (short-array [0 -1 32768 0 0 0])})]
    (is (= 0.0 (double (aget ^floats (:data sc) 0))))
    (is (< (Math/abs (- 1.0 (aget ^floats (:data sc) 1))) 1e-6) "0xFFFF reads as unsigned: 1.0")
    (is (< (Math/abs (- 0.50000763 (aget ^floats (:data sc) 2))) 1e-6))))

(deftest display-conversion
  (testing "clamps over-range and negative values"
    (is (= [[255 255 255] [0 0 0]] (map rgb (argb-of (grey-scene 4.0 -0.5))))))
  (testing "applies the sRGB curve: 18% grey -> 118"
    (let [[r g b] (rgb (first (argb-of (grey-scene 0.18))))]
      (is (= r g b)) (is (<= 117 r 119))))
  (testing "alpha is opaque"
    (is (= 255 (ch (first (argb-of (grey-scene 0.3))) 24)))))

(deftest output-spaces
  (let [red (scene/from-argb (core/image 1 1 (int-array [(px 255 0 0)])))
        [r g b] (rgb (first (argb-of red :display-p3)))]
    (testing "sRGB red expressed in Display P3 is ~(234, 51, 35)"
      (is (<= 232 r 236)) (is (<= 48 g 54)) (is (<= 32 b 38))))
  (testing "white stays white in every output space"
    (let [white (scene/from-argb (core/image 1 1 (int-array [(px 255 255 255)])))]
      (doseq [sp [:srgb :display-p3 :adobe-rgb]]
        (is (= [255 255 255] (rgb (first (argb-of white sp)))) (str sp))))))

(deftest sixteen-bit-output
  (let [out (scene/->encoded16 (grey-scene 0.0 1.0 0.21404114) :srgb)
        v   (fn [i] (bit-and (aget ^shorts (:data out) i) 0xFFFF))]
    (is (= [3 1] [(:width out) 1]))
    (is (= 9 (alength ^shorts (:data out))))
    (is (= 0 (v 0)))
    (is (= 65535 (v 3)))
    (is (< (Math/abs (- 32768 (v 6))) 20) "linear 21.4% = encoded 0.5")))

(deftest lookup-tables
  (let [lut (scene/make-lut #(* 2.0 %) 4)]
    (is (= 5 (alength lut)))
    (is (= 1.0 (scene/lut-at lut 0.5)))
    (is (< (Math/abs (- 0.5 (scene/lut-at lut 0.25))) 1e-12) "interpolates between samples")
    (is (= [0.0 2.0] [(scene/lut-at lut -3.0) (scene/lut-at lut 9.0)]) "clamps the index")))

(deftest extended-encoding
  (doseq [x [0.0 0.002 0.18 0.5 1.0 1.5 4.0]]
    (is (< (Math/abs (- x (scene/srgb-decode-extended (scene/srgb-encode-extended x)))) (+ 2e-4 (* 2e-4 x))) (str x)))
  (is (> (scene/srgb-encode-extended 2.0) 1.0) "over-range survives the perceptual domain")
  (is (= 0.0 (scene/srgb-encode-extended -1.0))))

(deftest fit-box-averages-in-linear-light
  (let [sc (scene/image 4 2 (float-array (mapcat (fn [v] [v v v]) [0 1 0 1 1 0 1 0])))
        f  (scene/fit sc 2)]
    (is (= [2 1] [(:width f) (:height f)]))
    (is (every? #(< (Math/abs (- 0.5 (double %))) 1e-6) (:data f)))
    (is (identical? sc (scene/fit sc 10)) "already small enough")))
