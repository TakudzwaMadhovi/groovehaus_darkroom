(ns darkroom.imaging.browser-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.dng :refer [write-dng!]]
            [darkroom.imaging.browser :as browser]
            [darkroom.imaging.core :as core]
            [darkroom.imaging.export :as export])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- tmp-dir ^File [] (.toFile (Files/createTempDirectory "darkroom-browser" (into-array FileAttribute []))))
(defn- touch! [^File dir n] (let [f (File. dir ^String n)] (spit f "x") f))
(defn- names [fs] (mapv #(.getName ^File %) fs))

(deftest scan-filters-and-sorts-naturally
  (let [d (tmp-dir)]
    (doseq [n ["IMG_10.jpg" "IMG_2.JPG" "IMG_1.png" "a.ARW" "b.dng" "notes.txt" ".hidden.jpg" "._IMG_3.jpg" "z.cr3"]]
      (touch! d n))
    (.mkdir (File. d "sub.jpg"))               ; a directory named like an image
    (is (= ["a.ARW" "b.dng" "IMG_1.png" "IMG_2.JPG" "IMG_10.jpg" "z.cr3"] (names (browser/scan d))))
    (testing "not recursive; missing / non-directory paths give []"
      (touch! (File. d "sub.jpg") "inner.jpg")
      (is (= 6 (count (browser/scan d))))
      (is (= [] (browser/scan (File. d "nope"))))
      (is (= [] (browser/scan (File. d "a.ARW")))))))

(deftest thumbnails
  (let [d (tmp-dir)
        w 1200 h 800
        big (core/image w h (int-array (* w h) (unchecked-int 0xFF3366CC)))]
    (export/save! big {:dir (str d) :name "big" :format :png})
    (export/save! big {:dir (str d) :name "bigj" :format :jpeg :quality 0.8})
    (testing "PNG and JPEG shrink to <= max-side, keeping aspect ratio"
      (doseq [n ["big.png" "bigj.jpg"]]
        (let [t (browser/thumbnail (File. d ^String n) 160)]
          (is (<= (max (:width t) (:height t)) 160))
          (is (> (max (:width t) (:height t)) 80))
          (is (< 1.3 (/ (double (:width t)) (:height t)) 1.7)))))
    (testing "small images are not enlarged"
      (let [small (core/image 40 30 (int-array 1200 (unchecked-int 0xFF000000)))
            _ (export/save! small {:dir (str d) :name "small" :format :png})
            t (browser/thumbnail (File. d "small.png") 160)]
        (is (= [40 30] [(:width t) (:height t)]))))
    (testing "RAW thumbnail"
      (let [t (browser/thumbnail (write-dng! 256 192 20000) 64)]
        (is (<= (max (:width t) (:height t)) 64))))
    (testing "corrupt file throws"
      (is (thrown? Exception (browser/thumbnail (touch! d "bad.jpg") 64))))))
