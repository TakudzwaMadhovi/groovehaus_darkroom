(ns darkroom.imaging.exif-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.dng :refer [write-dng!]]
            [darkroom.imaging.core :as core]
            [darkroom.imaging.exif :as exif]
            [darkroom.imaging.export :as export])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- tmp-dir [] (str (Files/createTempDirectory "darkroom-exif" (into-array FileAttribute []))))

(def ^:private tags
  {:make "ACME" :model "Cam 1" :copyright "(c) Someone" :artist "A. Person"
   :datetime-original "2024:05:01 10:20:30" :lens-model "50mm f/1.8"
   :exposure-time [1 250] :f-number [28 10] :focal-length [500 10] :exposure-bias [-1 3] :iso 400})

(deftest exif-block-round-trips-through-a-jpeg
  (let [dir (tmp-dir)
        img (core/image 8 8 (int-array 64 (unchecked-int 0xFF808080)))
        f   (export/save! img {:dir dir :name "t" :format :jpeg :quality 0.9
                               :exif (exif/exif-block tags {:srgb? true})})
        back (exif/read-tags f)]
    (doseq [[k v] tags]
      (is (= v (back k)) (str k)))
    (is (= "Groovehaus Darkroom" (:software back)))
    (testing "pixels are upright, so orientation is written as 1"
      (is (= 1 (exif/orientation f))))))

(deftest block-layout
  (let [^bytes b (exif/exif-block {} {})]
    (is (= "Exif" (String. b 0 4 "US-ASCII")))
    (is (= [0 0] [(aget b 4) (aget b 5)]))
    (is (= "II" (String. b 6 2 "US-ASCII")) "little-endian TIFF follows")
    (is (< (alength b) 200) "no tags beyond Software, Orientation, ColorSpace")))

(deftest read-tags-from-files-without-metadata
  (let [dir (tmp-dir)
        f (export/save! (core/image 4 4 (int-array 16 (unchecked-int 0xFF000000))) {:dir dir :name "plain" :format :png})]
    (is (= {} (exif/read-tags f)))
    (is (= {} (exif/read-tags (str dir "/missing.jpg"))) "unreadable file: empty, not an exception")))

(deftest read-tags-from-raw
  (let [t (exif/read-tags (write-dng! 32 32 20000))]
    (is (= "Test" (:make t)))
    (is (= "Synthetic" (:model t)))))
