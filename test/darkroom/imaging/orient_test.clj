(ns darkroom.imaging.orient-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.browser :as browser]
            [darkroom.imaging.core :as core]
            [darkroom.imaging.exif :as exif]
            [darkroom.imaging.export :as export]
            [darkroom.imaging.loader :as loader])
  (:import (java.io ByteArrayOutputStream File)
           (java.nio ByteBuffer ByteOrder)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

;; 3 wide x 2 tall; pixel value = its (row-major) index + 1:
;;   1 2 3
;;   4 5 6
(def ^:private src (core/image 3 2 (int-array [1 2 3 4 5 6])))

(defn- grid [img]
  (let [{:keys [width height pixels]} img]
    (vec (for [y (range height)] (vec (for [x (range width)] (aget ^ints pixels (+ (* y width) x))))))))

(deftest orient-all-eight
  (is (= [[1 2 3] [4 5 6]] (grid (core/orient src 1))))
  (is (= [[3 2 1] [6 5 4]] (grid (core/orient src 2))) "mirror horizontal")
  (is (= [[6 5 4] [3 2 1]] (grid (core/orient src 3))) "rotate 180")
  (is (= [[4 5 6] [1 2 3]] (grid (core/orient src 4))) "mirror vertical")
  (is (= [[1 4] [2 5] [3 6]] (grid (core/orient src 5))) "transpose")
  (is (= [[4 1] [5 2] [6 3]] (grid (core/orient src 6))) "rotate 90 CW")
  (is (= [[6 3] [5 2] [4 1]] (grid (core/orient src 7))) "transverse")
  (is (= [[3 6] [2 5] [1 4]] (grid (core/orient src 8))) "rotate 90 CCW")
  (testing "absent / unknown values are a no-op"
    (is (identical? src (core/orient src nil)))
    (is (identical? src (core/orient src 9)))))

(deftest orient-round-trips
  (is (= (grid src) (grid (core/orient (core/orient src 6) 8))) "CW then CCW")
  (is (= (grid src) (grid (core/orient (core/orient src 3) 3))) "180 twice")
  (is (= (grid src) (grid (core/orient (core/orient src 5) 5))) "transpose twice"))

;; --- real files: a JPEG whose EXIF says "rotate 90 CW" ---------------------

(defn- exif-segment
  "APP1/Exif segment holding only an Orientation tag."
  ^bytes [orientation]
  (let [bb (doto (ByteBuffer/allocate 36) (.order ByteOrder/BIG_ENDIAN))]
    (.putShort bb (unchecked-short 0xFFE1))
    (.putShort bb (unchecked-short 34))          ; segment length (excludes marker)
    (.put bb (.getBytes "Exif\u0000\u0000" "ISO-8859-1"))
    (.order bb ByteOrder/LITTLE_ENDIAN)
    (.put bb (.getBytes "II" "ISO-8859-1")) (.putShort bb (short 42)) (.putInt bb 8)
    (.putShort bb (short 1))                     ; one IFD entry
    (.putShort bb (short 0x0112)) (.putShort bb (short 3)) (.putInt bb 1)
    (.putShort bb (short orientation)) (.putShort bb (short 0))
    (.putInt bb 0)
    (.array bb)))

(defn- jpeg-with-orientation!
  "Writes a JPEG of `img` into `dir` with the given EXIF orientation tag."
  ^File [dir img orientation]
  (let [plain (export/save! img {:dir dir :name (str "plain" orientation) :format :jpeg :quality 1.0})
        bytes (Files/readAllBytes (.toPath plain))
        out   (File. (str dir) (str "exif" orientation ".jpg"))
        baos  (ByteArrayOutputStream.)]
    (.write baos bytes 0 2)                      ; SOI
    (.write baos (exif-segment orientation))
    (.write baos bytes 2 (- (alength bytes) 2))
    (Files/write (.toPath out) (.toByteArray baos) (into-array java.nio.file.OpenOption []))
    out))

(defn- tmp-dir [] (str (Files/createTempDirectory "darkroom-orient" (into-array FileAttribute []))))

(defn- solid [w h argb] (core/image w h (int-array (* w h) (unchecked-int argb))))

(defn- landscape
  "200x100: left half red, right half blue."
  []
  (let [w 200 h 100 px (int-array (* w h))]
    (dotimes [y h] (dotimes [x w] (aset px (+ (* y w) x) (unchecked-int (if (< x 100) 0xFFFF0000 0xFF0000FF)))))
    (core/image w h px)))

(defn- dominant [p] ; :red / :blue
  (let [r (bit-and (unsigned-bit-shift-right p 16) 0xFF) b (bit-and p 0xFF)]
    (if (> r b) :red :blue)))

(defn- px-at [img x y] (aget ^ints (:pixels img) (+ (* y (:width img)) x)))

(deftest reads-exif-orientation
  (let [d (tmp-dir)]
    (is (= 6 (exif/orientation (jpeg-with-orientation! d (landscape) 6))))
    (is (= 3 (exif/orientation (jpeg-with-orientation! d (landscape) 3))))
    (testing "no EXIF / missing file / garbage -> 1"
      (is (= 1 (exif/orientation (export/save! (landscape) {:dir d :name "none" :format :jpeg}))))
      (is (= 1 (exif/orientation (File. d "missing.jpg")))))))

(deftest loader-and-thumbnails-apply-orientation
  (let [d (tmp-dir)
        f (jpeg-with-orientation! d (landscape) 6)]   ; rotate 90 CW -> 100 wide x 200 tall
    (testing "full load is upright: portrait, red now on top, blue at bottom"
      (let [i (loader/load-image f)]
        (is (= [100 200] [(:width i) (:height i)]))
        (is (= :red (dominant (px-at i 50 20))))
        (is (= :blue (dominant (px-at i 50 180))))))
    (testing "thumbnail matches the full image's orientation"
      (let [t (browser/thumbnail f 50)]
        (is (< (:width t) (:height t)))
        (is (= :red (dominant (px-at t (quot (:width t) 2) 2))))
        (is (= :blue (dominant (px-at t (quot (:width t) 2) (- (:height t) 3)))))))
    (testing "files without the tag are untouched"
      (let [i (loader/load-image (export/save! (landscape) {:dir d :name "plain" :format :png}))]
        (is (= [200 100] [(:width i) (:height i)]))))))
