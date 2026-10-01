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
