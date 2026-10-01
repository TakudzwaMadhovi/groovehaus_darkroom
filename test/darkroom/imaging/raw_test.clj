(ns darkroom.imaging.raw-test
  "Decodes a synthetic DNG (see darkroom.dng) with LibRaw into linear RGB."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [darkroom.dng :refer [write-dng!]]
            [darkroom.imaging.core :as core]
            [darkroom.imaging.export :as export]
            [darkroom.imaging.loader :as loader]
            [darkroom.imaging.raw :as raw])
  (:import (java.io File)))

(defn- sample ^long [^shorts data ^long i] (bit-and (aget data (int i)) 0xFFFF))

(deftest recognises-raw-extensions
  (is (raw/raw-file? "a/b/IMG_0001.ARW"))
  (is (raw/raw-file? "x.dng"))
  (is (not (raw/raw-file? "photo.jpg")))
  (is (not (raw/raw-file? "dng"))))

(deftest libraw-is-loaded
  (is (re-find #"^0\.21" (raw/libraw-version))))

(deftest decodes-synthetic-dng-to-linear-rgb
  (let [f   (write-dng! 64 48 16384)         ; 25% of full scale
        img (raw/decode-linear f)
        mid (* 3 (+ (* 24 64) 32))]
    (is (= [64 48 3 16] [(:width img) (:height img) (:channels img) (:bits img)]))
    (is (= (* 64 48 3) (alength ^shorts (:data img))))
    (testing "values are scene-linear: ~25% grey stays ~25% (not gamma-encoded)"
      (doseq [c [0 1 2]]
        (is (< (Math/abs (- 16384 (sample (:data img) (+ mid c)))) 1500))))
    (testing "display conversion applies the sRGB curve: 25% linear -> ~137/255"
      (let [d (raw/linear->display img)
            p (aget ^ints (:pixels d) (+ (* 24 64) 32))]
        (is (= 255 (bit-and (unsigned-bit-shift-right p 24) 0xFF)))
        (is (< (Math/abs (- 137 (bit-and (unsigned-bit-shift-right p 8) 0xFF))) 8))))))

(deftest linear-response-doubles-with-light
  (let [lo (raw/decode-linear (write-dng! 32 32 8000))
        hi (raw/decode-linear (write-dng! 32 32 16000))
        i  (* 3 (+ (* 16 32) 16))
        ratio (/ (double (sample (:data hi) i)) (double (sample (:data lo) i)))]
    (is (< 1.9 ratio 2.1))))

(deftest loader-dispatches-and-errors-cleanly
  (let [d (loader/load-image (write-dng! 32 32 20000))]
    (is (= [32 32] [(:width d) (:height d)])))
  (is (= [800 600] (let [i (loader/load-image "resources/sample.png")] [(:width i) (:height i)])))
  (let [bad (File/createTempFile "darkroom-bad" ".arw")]
    (.deleteOnExit bad)
    (spit bad "not a raw file")
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Cannot (open|unpack)" (raw/decode-linear bad)))))

(defn- red-over-blue-jpeg
  "120x80 JPEG: top half red, bottom half blue."
  ^bytes []
  (let [w 120 h 80 px (int-array (* w h))]
    (dotimes [y h] (dotimes [x w] (aset px (+ (* y w) x) (unchecked-int (if (< y 40) 0xFFFF0000 0xFF0000FF)))))
    (let [f (File/createTempFile "thumb" ".jpg")]
      (.deleteOnExit f)
      (export/save! (core/image w h px)
                                     {:dir (str (.getParent f)) :name (str/replace (.getName f) #"\.jpg$" "") :format :jpeg :quality 1.0})
      (java.nio.file.Files/readAllBytes (.toPath f)))))

(defn- dom [p] (if (> (bit-and (unsigned-bit-shift-right p 16) 0xFF) (bit-and p 0xFF)) :red :blue))
(defn- at [img x y] (aget ^ints (:pixels img) (+ (* y (:width img)) x)))

(deftest embedded-thumbnail-extraction
  (testing "a DNG without a preview yields nil"
    (is (nil? (raw/embedded-thumbnail (write-dng! 64 48 20000) 32))))
  (let [jpg (red-over-blue-jpeg)
        f   (write-dng! 64 48 20000 {:thumb jpg :thumb-size [120 80]})]
    (testing "the embedded JPEG is returned at its own size"
      (let [t (raw/embedded-thumbnail f 64)]
        (is (= [120 80] [(:width t) (:height t)]))
        (is (= :red (dom (at t 60 10))))
        (is (= :blue (dom (at t 60 70))))))
    (testing "too-small previews are rejected so callers can fall back"
      (is (nil? (raw/embedded-thumbnail f 400))))
    (testing "the raw data still decodes beside the preview"
      (is (= [64 48] (let [i (raw/decode-linear f)] [(:width i) (:height i)]))))))

(deftest embedded-thumbnail-respects-orientation
  (let [f (write-dng! 64 48 20000 {:thumb (red-over-blue-jpeg) :thumb-size [120 80] :orientation 6})
        t (raw/embedded-thumbnail f 64)]
    (testing "orientation 6 (rotate 90 CW) turns 120x80 into 80x120, red now on the right"
      (is (= [80 120] [(:width t) (:height t)]))
      (is (= :blue (dom (at t 10 60))))
      (is (= :red (dom (at t 70 60)))))))
