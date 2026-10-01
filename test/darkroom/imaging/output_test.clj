(ns darkroom.imaging.output-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.output :as out]
            [darkroom.imaging.pipeline :as pipeline]
            [darkroom.imaging.scene :as scene])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (javax.imageio ImageIO)))

(defn- tmp ^File [] (.toFile (Files/createTempDirectory "darkroom-out" (into-array FileAttribute []))))

(defn- png! ^String [^File dir n w h]
  (let [f (File. dir ^String n)
        b (java.awt.image.BufferedImage. w h java.awt.image.BufferedImage/TYPE_INT_RGB)]
    (dotimes [y h] (dotimes [x w] (.setRGB b x y (bit-or (bit-shift-left (quot (* 255 x) w) 16) (bit-shift-left (quot (* 255 y) h) 8) 90))))
    (ImageIO/write b "png" f)
    (.getPath f)))

(deftest templates
  (let [v {:name "img_7" :n 3 :date "2024-05-01" :rating 4 :shoot "Beach"}]
    (is (= "img_7" (out/expand-template "{name}" v)))
    (is (= "2024-05-01_beach_003_4" (out/expand-template "{date}_beach_{n3}_{rating}" v)))
    (is (= "Beach-3" (out/expand-template "{shoot}-{n}" v)))
    (is (= "a{nope}" (out/expand-template "a{nope}" v)) "unknown tokens stay")
    (testing "cannot leave the destination folder"
      (is (= "_x" (out/expand-template "../{name}" {:name "x"})) "separators replaced, leading dots dropped")
      (is (not (re-find #"^\." (out/expand-template "...hidden" v)))))
    (is (= "img_7" (out/expand-template "  " v)) "a blank result falls back to the name")))

(deftest capture-dates
  (is (= "2024-05-01" (out/capture-date {:datetime-original "2024:05:01 10:00:00"})))
  (is (re-matches #"\d{4}-\d{2}-\d{2}" (out/capture-date {}))))

(deftest metadata-modes
  (let [tags {:make "Canon" :artist "Cam" :copyright "(c) cam" :iso 100}
        mine {:creator "Me" :copyright ""}]
    (is (= (assoc tags :artist "Me") (out/export-tags :all tags mine)) "own creator wins, blank copyright does not")
    (is (= {:artist "Me" :copyright "(c) cam"} (out/export-tags :copyright tags mine)))
    (is (nil? (out/export-tags :none tags mine)))))

(deftest sharpening-for-output
  (let [w 32 h 8
        edge (scene/image w h (let [a (float-array (* 3 w h))]
                                (dotimes [i (* w h)] (let [v (if (< (rem i w) 16) 0.2 0.6)] (dotimes [c 3] (aset a (+ (* 3 i) c) (float v)))))
                                a))
        flat (scene/image 8 8 (let [a (float-array 192)] (java.util.Arrays/fill a (float 0.4)) a))
        px (fn [img x] (aget ^floats (:data img) (* 3 (+ (* 4 (:width img)) x))))]
    (is (identical? edge (out/sharpen-output edge nil)))
    (let [s (out/sharpen-output edge {:for :screen :amount :standard})]
      (is (< (px s 14) (px edge 14)) "dark side of the edge gets darker")
      (is (> (px s 17) (px edge 17)) "bright side gets brighter"))
    (let [more (out/sharpen-output edge {:for :glossy :amount :high})
          less (out/sharpen-output edge {:for :screen :amount :low})]
      (is (> (- (px more 17) (px more 14)) (- (px less 17) (px less 14))) "stronger setting, stronger edge"))
    (let [s (out/sharpen-output flat {:for :matte :amount :high})]
      (is (every? #(< (Math/abs (- % 0.4)) 1e-4) (:data s)) "flat areas are untouched"))))

(deftest watermark-marks-only-its-corner
  (let [w 400 h 300
        base (scene/image w h (let [a (float-array (* 3 w h))] (java.util.Arrays/fill a (float 0.1)) a))
        marked (out/watermark base {:text "GROOVEHAUS" :position :bottom-right :opacity 1.0 :size 0.06})
        changed (fn [x y] (not= (aget ^floats (:data base) (* 3 (+ (* y w) x))) (aget ^floats (:data marked) (* 3 (+ (* y w) x)))))
        pts (for [y (range h) x (range w) :when (changed x y)] [x y])]
    (is (seq pts) "something was drawn")
    (is (every? (fn [[x y]] (and (> x (* 0.4 w)) (> y (* 0.8 h)))) pts) "all of it in the bottom-right corner")
    (is (some (fn [[x y]] (> (aget ^floats (:data marked) (* 3 (+ (* y w) x))) 0.5)) pts) "text is light")
    (is (identical? base (out/watermark base {:text "  "})))
    (let [tl (out/watermark base {:text "X" :position :top-left :opacity 1.0 :size 0.1})]
      (is (not= (seq (:data base)) (seq (:data tl)))))))

(deftest exports-a-frame-with-everything-applied
  (let [src (tmp) dst (tmp) p (png! src "Beach.png" 120 80)
        f (out/export-frame! {:path p :adj pipeline/default-settings :n 2 :shoot "S" :rating 3 :meta {:creator "Ann"}}
                             {:dir (str dst) :template "{shoot}_{name}_{n}" :format :jpeg :size 60 :space :display-p3
                              :sharpen {:for :screen :amount :low} :watermark {:text "hi" :opacity 0.5}})]
    (is (= "S_beach_2.jpg" (.getName f)))
    (let [i (ImageIO/read f)] (is (= [60 40] [(.getWidth i) (.getHeight i)])))
    (testing "a second export never overwrites"
      (is (= "S_beach_2-2.jpg" (.getName (out/export-frame! {:path p :adj pipeline/default-settings :n 2 :shoot "S"}
                                                            {:dir (str dst) :template "{shoot}_{name}_{n}" :format :jpeg :size 60})))))
    (testing "tiff and webp go through the same path"
      (is (= "groovehaus_beach.tif" (.getName (out/export-frame! {:path p :adj pipeline/default-settings} {:dir (str dst) :format :tiff :size 0 :space :adobe-rgb}))))
      (is (= "groovehaus_beach.webp" (.getName (out/export-frame! {:path p :adj pipeline/default-settings} {:dir (str dst) :format :webp :size 40 :space :display-p3})))))))

(deftest batch-numbers-frames-and-reports-failures
  (let [src (tmp) dst (tmp) a (png! src "a.png" 30 20) b (png! src "b.png" 30 20)
        calls (atom [])
        r (out/export-frames! [{:path a :adj pipeline/default-settings}
                               {:path (str (File. src "missing.png")) :adj pipeline/default-settings}
                               {:path b :adj pipeline/default-settings}]
                              {:dir (str dst) :template "{n3}-{name}" :size 0}
                              (fn [i n] (swap! calls conj [i n])))]
    (is (= ["001-a.jpg" "003-b.jpg"] (mapv #(.getName %) (:files r))))
    (is (= 1 (count (:failed r))))
    (is (= [[1 3] [2 3] [3 3]] @calls))))
