(ns darkroom.imaging.exif
  "Reads EXIF metadata and writes a minimal EXIF block for exported files.
  Pure I/O, no UI dependency."
  (:require [darkroom.imaging.paths :as paths])
  (:import (com.drew.imaging ImageMetadataReader)
           (com.drew.lang Rational)
           (com.drew.metadata Directory Metadata)
           (com.drew.metadata.exif ExifIFD0Directory ExifSubIFDDirectory)
           (java.io File)
           (java.nio ByteBuffer ByteOrder)))

(defn orientation
  "EXIF orientation (1-8) of an image file, or 1 when absent or unreadable.
  Phones and cameras store sensor-orientation pixels plus this tag; ImageIO
  ignores it, so callers apply it with darkroom.imaging.core/orient."
  [file]
  (try
    (let [dir (.getFirstDirectoryOfType (ImageMetadataReader/readMetadata (File. (paths/source-file file)))
                                        ExifIFD0Directory)]
      (if (and dir (.containsTag dir ExifIFD0Directory/TAG_ORIENTATION))
        (let [o (.getInt dir ExifIFD0Directory/TAG_ORIENTATION)]
          (if (<= 1 o 8) o 1))
        1))
    (catch Throwable _ 1)))

;; ------------------------------------------------------------------ reading

(def ^:private ifd0-tags
  "[key tag type]; type :ascii"
  [[:make 0x010F] [:model 0x0110] [:software 0x0131] [:datetime 0x0132]
   [:artist 0x013B] [:copyright 0x8298]])

(def ^:private exif-tags
  [[:datetime-original 0x9003 :ascii] [:lens-make 0xA433 :ascii] [:lens-model 0xA434 :ascii]
   [:exposure-time 0x829A :rational] [:f-number 0x829D :rational]
   [:focal-length 0x920A :rational] [:exposure-bias 0x9204 :srational]
   [:iso 0x8827 :short]])

(defn- rational [^Rational r] [(.getNumerator r) (.getDenominator r)])

(defn read-tags
  "The camera metadata worth carrying into an export, as a map (any of :make
  :model :software :datetime :artist :copyright :datetime-original :lens-make
  :lens-model :exposure-time :f-number :focal-length :exposure-bias [n d]
  rationals, :iso). Works for JPEG and RAW. Missing or unreadable metadata
  yields {}."
  [file]
  (try
    (let [^Metadata md (ImageMetadataReader/readMetadata (File. (paths/source-file file)))
          d0  (.getFirstDirectoryOfType md ExifIFD0Directory)
          sub (.getFirstDirectoryOfType md ExifSubIFDDirectory)
          get-in-dir (fn [^Directory d [k tag type]]
                       (when (and d (.containsTag d (int tag)))
                         (when-let [v (case type
                                        :ascii (not-empty (.trim (str (.getString d (int tag)))))
                                        (:rational :srational) (some-> (.getRational d (int tag)) rational)
                                        :short (.getInteger d (int tag)))]
                           [k v])))]
      (into {} (concat (keep #(get-in-dir d0 (conj % :ascii)) ifd0-tags)
                       (keep #(get-in-dir sub %) exif-tags))))
    (catch Throwable _ {})))

;; ------------------------------------------------------------------ writing

(defn- payload
  "[count bytes] of a TIFF field value."
  [type v]
  (case type
    :ascii (let [b (.getBytes (str v) "US-ASCII")] [(inc (alength b)) (java.util.Arrays/copyOf b (inc (alength b)))])
    :short [1 (-> (ByteBuffer/allocate 2) (.order ByteOrder/LITTLE_ENDIAN) (.putShort (unchecked-short (long v))) .array)]
    :long  [1 (-> (ByteBuffer/allocate 4) (.order ByteOrder/LITTLE_ENDIAN) (.putInt (unchecked-int (long v))) .array)]
    (:rational :srational)
    [1 (-> (ByteBuffer/allocate 8) (.order ByteOrder/LITTLE_ENDIAN)
           (.putInt (unchecked-int (long (first v)))) (.putInt (unchecked-int (long (second v)))) .array)]))

(def ^:private type-code {:ascii 2 :short 3 :long 4 :rational 5 :srational 10})

(defn- build-tiff
  "Little-endian TIFF with IFD0 (+ a pointer to the EXIF IFD) and the EXIF IFD.
  Each IFD is a sorted seq of [tag type value]."
  ^bytes [ifd0 exif-ifd]
  (let [ifd0   (sort-by first ifd0)
        exif-ifd (sort-by first exif-ifd)
        n0     (+ (count ifd0) (if (seq exif-ifd) 1 0))
        n1     (count exif-ifd)
        ifd-sz (fn [n] (if (pos? n) (+ 2 (* 12 n) 4) 0))
        exif-off (+ 8 (ifd-sz n0))
        data-off (+ exif-off (ifd-sz n1))
        data   (java.io.ByteArrayOutputStream.)
        entry  (fn [[tag type v]]
                 (let [[cnt ^bytes bs] (payload type v)]
                   (if (<= (alength bs) 4)
                     [tag type cnt (java.util.Arrays/copyOf bs 4)]
                     (let [off (+ data-off (.size data))]
                       (.write data bs 0 (alength bs))
                       (when (odd? (alength bs)) (.write data 0))
                       [tag type cnt (-> (ByteBuffer/allocate 4) (.order ByteOrder/LITTLE_ENDIAN) (.putInt (int off)) .array)]))))
        write-ifd (fn [^ByteBuffer bb entries]
                    (.putShort bb (short (count entries)))
                    (doseq [[tag type cnt ^bytes val] entries]
                      (.putShort bb (unchecked-short tag)) (.putShort bb (short (type-code type)))
                      (.putInt bb (int cnt)) (.put bb val))
                    (.putInt bb 0))
        e0 (mapv entry ifd0)
        e1 (mapv entry exif-ifd)
        e0 (if (seq exif-ifd)
             (vec (sort-by first (conj e0 [0x8769 :long 1 (-> (ByteBuffer/allocate 4) (.order ByteOrder/LITTLE_ENDIAN) (.putInt (int exif-off)) .array)])))
             e0)
        bb (-> (ByteBuffer/allocate (+ data-off (.size data))) (.order ByteOrder/LITTLE_ENDIAN))]
    (.put bb (.getBytes "II" "US-ASCII")) (.putShort bb (short 42)) (.putInt bb 8)
    (write-ifd bb e0)
    (when (seq e1) (write-ifd bb e1))
    (.put bb (.toByteArray data))
    (.array bb)))

(defn exif-block
  "The APP1 payload ('Exif\\0\\0' + TIFF) for an exported file: the camera
  `tags` from `read-tags` (the pixels are already upright, so orientation is 1),
  Software, and the ColorSpace flag (1 = sRGB, 65535 = uncalibrated, i.e.
  described by the embedded ICC profile)."
  ^bytes [tags {:keys [software srgb?] :or {software "Groovehaus Darkroom"}}]
  (let [t (fn [ks spec] (for [[k tag type] spec :when (contains? ks k)] [tag type (ks k)]))
        ifd0 (concat (for [[k tag] ifd0-tags :when (contains? tags k) :when (not= k :software)]
                       [tag :ascii (tags k)])
                     [[0x0131 :ascii software] [0x0112 :short 1]])
        exif (concat (t tags exif-tags) [[0xA001 :short (if srgb? 1 65535)]])
        tiff (build-tiff ifd0 exif)]
    (let [out (java.io.ByteArrayOutputStream.)]
      (.write out (.getBytes "Exif" "US-ASCII")) (.write out 0) (.write out 0)
      (.write out tiff 0 (alength tiff))
      (.toByteArray out))))
