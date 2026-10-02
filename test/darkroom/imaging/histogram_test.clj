(ns darkroom.imaging.histogram-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.core :as core]
            [darkroom.imaging.histogram :as h]
            [darkroom.imaging.pipeline :as pipeline]
            [darkroom.imaging.scene :as scene]))

(defn- px [r g b] (unchecked-int (bit-or 0xFF000000 (bit-shift-left r 16) (bit-shift-left g 8) b)))
(defn- img [& ps] (core/image (count ps) 1 (int-array ps)))

(deftest counts
  (let [{:keys [r g b luma count]} (h/compute (img (px 10 20 30) (px 10 200 255) (px 0 0 0)))]
    (is (= 3 count))
    (is (= [2 1] [(aget ^longs r 10) (aget ^longs r 0)]))
    (is (= [1 1 1] [(aget ^longs g 20) (aget ^longs g 200) (aget ^longs g 0)]))
    (is (= 1 (aget ^longs b 255)))
    (testing "every channel's bins sum to the pixel count"
      (is (every? #(= 3 (reduce + (seq %))) [r g b luma])))
    (testing "luma of pure white is 255, black is 0"
      (is (= 1 (aget ^longs (:luma (h/compute (img (px 255 255 255)))) 255)))
      (is (= 1 (aget ^longs luma 0))))))

(deftest tracks-adjustments
  (let [i (img (px 100 100 100) (px 100 100 100))
        before (h/compute i)
        after  (h/compute (scene/->argb (pipeline/render (scene/from-argb i) {:exposure 0.5})))]
    (is (= 2 (aget ^longs (:r before) 100)))
    (is (= 0 (aget ^longs (:r after) 100)))
    (testing "+0.5 stop = x1.41 in linear light: sRGB 100 -> 118"
      (is (= 2 (reduce + (map #(aget ^longs (:r after) %) [117 118 119])))))))

(deftest clipping-counts
  (let [h (h/compute (img (px 255 10 10) (px 10 0 200) (px 255 255 255) (px 0 0 0) (px 100 100 100) (px 128 129 130)))]
    (is (= 2 (:clip-high h)) "any channel at 255: the red-ish pixel and white")
    (is (= 2 (:clip-low h)) "any channel at 0: the blue-ish pixel (green 0) and black")
    (is (= 6 (:count h))))
  (is (= [0 0] [(:clip-low (h/compute (img (px 5 5 5)))) (:clip-high (h/compute (img (px 5 5 5))))])))

(deftest clipping-percentages
  (let [h (h/compute (img (px 255 255 255) (px 0 0 0) (px 9 9 9) (px 8 8 8)))]
    (is (= [25.0 25.0] (h/clip-percentages h))))
  (is (= [0.0 0.0] (h/clip-percentages (h/compute (core/image 0 0 (int-array 0)))))))

(deftest clipping-overlay-paints-clipped-pixels
  (let [src (img (px 255 10 10) (px 10 0 200) (px 100 100 100) (px 255 0 0))
        out (h/clipping-overlay src)
        ps (vec (:pixels out))]
    (is (= [4 1] [(:width out) (:height out)]))
    (is (= h/clip-high-colour (ps 0)) "highlight clipping is red")
    (is (= h/clip-low-colour (ps 1)) "shadow clipping is blue")
    (is (= (px 100 100 100) (ps 2)) "unclipped pixels are untouched")
    (is (= h/clip-high-colour (ps 3)) "highlights win when a pixel clips both ways")
    (is (= (px 255 10 10) (aget ^ints (:pixels src) 0)) "the source is not modified")))
