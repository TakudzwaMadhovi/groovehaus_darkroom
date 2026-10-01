(ns darkroom.imaging.browser
  "Folder scanning and thumbnail generation for the file browser. Pure logic /
  I/O, no UI dependency."
  (:require [clojure.string :as str]
            [darkroom.imaging.core :as core]
            [darkroom.imaging.exif :as exif]
            [darkroom.imaging.raw :as raw])
  (:import (java.io File)
           (javax.imageio ImageIO)))

(def ^:private image-extensions
  (into (set (map str/lower-case (ImageIO/getReaderFileSuffixes))) raw/raw-extensions))

(defn- extension [^File f]
  (let [n (.toLowerCase (.getName f))
        i (.lastIndexOf n ".")]
    (when (pos? i) (subs n (inc i)))))

(defn supported-image?
  "True for a visible regular file whose extension ImageIO or LibRaw can read.
  Dot-files (including macOS '._' resource forks) are skipped."
  [^File f]
  (and (.isFile f)
       (not (.startsWith (.getName f) "."))
       (contains? image-extensions (extension f))))

(defn- natural-key
  "Splits a name into text / number chunks so IMG_2 sorts before IMG_10."
  [^File f]
  (mapv #(if (Character/isDigit (.charAt ^String % 0)) (Long/parseLong %) %)
        (re-seq #"\d{1,18}|\D+" (str/lower-case (.getName f)))))

(defn- compare-keys [a b]
  (loop [[x & xs :as as] (seq a) [y & ys :as bs] (seq b)]
    (cond
      (and (nil? as) (nil? bs)) 0
      (nil? as) -1
      (nil? bs) 1
      :else (let [c (cond (and (number? x) (number? y)) (compare x y)
                          (number? x) -1
                          (number? y) 1
                          :else (compare x y))]
              (if (zero? c) (recur xs ys) c)))))

(defn scan
  "Lists the supported images directly inside `dir` (not recursive), in natural
  name order. Returns [] if `dir` is not a readable directory."
  [dir]
  (let [^File d (File. (str dir))]
    (if-let [files (and (.isDirectory d) (.listFiles d))]
      (->> files
           (filter supported-image?)
           (sort-by natural-key compare-keys)
           vec)
      [])))

(defn- read-subsampled
  "Reads an ImageIO-supported file, letting the decoder skip pixels so large
  JPEGs are not fully decoded. The result is at least ~2x `max-side`."
  [^File f max-side]
  (with-open [in (ImageIO/createImageInputStream f)]
    (let [^java.util.Iterator readers (ImageIO/getImageReaders in)]
      (when-not (.hasNext readers)
        (throw (ex-info "Unreadable or unsupported image file" {:path (str f)})))
      (let [^javax.imageio.ImageReader r (.next readers)]
        (try
          (.setInput r in true true)
          (let [k     (max 1 (quot (max (.getWidth r 0) (.getHeight r 0)) (* 2 (long max-side))))
                param (doto (.getDefaultReadParam r) (.setSourceSubsampling k k 0 0))]
            (core/from-buffered (.read r 0 param)))
          (finally (.dispose r)))))))

(defn thumbnail
  "Returns an image map whose longest side is at most `max-side`.
  RAW files use the embedded JPEG preview when it is large enough, else
  LibRaw's fast half-size decode; other formats use a subsampled read, then the EXIF orientation is applied."
  [file max-side]
  (let [f (File. (str file))
        img (if (raw/raw-file? f)
              (or (raw/embedded-thumbnail f max-side)
                  (raw/load-image f {:half-size? true :quality 0}))
              (core/orient (read-subsampled f max-side) (exif/orientation f)))]
    (core/fit img max-side)))
