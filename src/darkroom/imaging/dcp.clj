(ns darkroom.imaging.dcp
  "Reader for DNG camera profiles (.dcp files, Adobe's DNG specification 1.4+
  chapter on camera profiles): the colour matrices and illuminants that turn a
  camera's raw colours into XYZ, and the optional hue / saturation / value
  tables, look table and tone curve that give a profile its rendering.

  A .dcp is a little-endian TIFF-style file whose header says `IIRC` instead of
  the usual 42. Pure logic, no UI dependency."
  (:import (java.nio ByteBuffer ByteOrder)))

(def ^:private tag-names
  {50708 :unique-camera-model
   50721 :color-matrix-1 50722 :color-matrix-2
   50778 :illuminant-1 50779 :illuminant-2
   50936 :name
   50937 :hue-sat-dims 50938 :hue-sat-data-1 50939 :hue-sat-data-2
   50940 :tone-curve
   50942 :copyright
   50964 :forward-matrix-1 50965 :forward-matrix-2
   50981 :look-dims 50982 :look-data
   51107 :hue-sat-encoding 51108 :look-encoding
   51109 :baseline-exposure})

(defn- read-values
  "Values of one directory entry: a vector (floats for type 11 stay in a float
  array: tables run to hundreds of thousands of entries)."
  [^ByteBuffer bb type n value-pos]
  (let [size ({1 1 2 1 3 2 4 4 5 8 6 1 7 1 8 2 9 4 10 8 11 4 12 8} type 1)
        total (* size n)
        pos (if (<= total 4) value-pos (.getInt bb (int value-pos)))
        b (doto (.duplicate bb) (.order ByteOrder/LITTLE_ENDIAN) (.position (int pos)))]
    (case (int type)
      (1 7) (vec (repeatedly n #(bit-and (.get b) 0xFF)))
      2     (let [bs (byte-array n)] (.get b bs) (String. bs 0 (max 0 (dec n)) "US-ASCII"))
      3     (vec (repeatedly n #(bit-and (.getShort b) 0xFFFF)))
      4     (vec (repeatedly n #(bit-and (.getInt b) 0xFFFFFFFF)))
      5     (vec (repeatedly n #(let [a (bit-and (.getInt b) 0xFFFFFFFF) d (bit-and (.getInt b) 0xFFFFFFFF)] (if (zero? d) 0.0 (/ (double a) d)))))
      10    (vec (repeatedly n #(let [a (.getInt b) d (.getInt b)] (if (zero? d) 0.0 (/ (double a) d)))))
      11    (let [fa (float-array n)] (.get (.asFloatBuffer b) fa) fa)
      12    (vec (repeatedly n #(.getDouble b)))
      (vec (repeat n 0)))))

(defn- rows [v] (when (and v (= 9 (count v))) (vec v)))

(defn- table [dims data encoding]
  (when (and dims data (= 3 (count dims)))
    (let [[h s v] (map long dims)]
      (when (and (pos? h) (pos? s) (pos? v) (= (* 3 h s v) (alength ^floats data)))
        {:dims (long-array [h s v]) :data data :encoding (long (or encoding 0))}))))

(defn parse
  "Parses the bytes of a .dcp. Returns
  {:name :camera :illuminant-1 :illuminant-2 (EXIF light-source codes)
   :color-matrix-1/2 and :forward-matrix-1/2 (9 numbers, row-major, or nil)
   :hue-sat-1 / :hue-sat-2 / :look ({:dims longs [hue sat val] :data floats :encoding 0|1} or nil)
   :tone-curve [[x y] ...] :baseline-exposure}.
  Throws ex-info for anything that is not a DCP or carries no colour matrix."
  [^bytes bs]
  (let [bb (doto (ByteBuffer/wrap bs) (.order ByteOrder/LITTLE_ENDIAN))]
    (when (or (< (alength bs) 16) (not= "IIRC" (String. bs 0 4 "US-ASCII")))
      (throw (ex-info "Not a DNG camera profile (.dcp)" {})))
    (let [ifd (.getInt bb 4)
          n (bit-and (.getShort bb (int ifd)) 0xFFFF)
          tags (into {} (for [i (range n)
                              :let [p (+ ifd 2 (* 12 i))
                                    tag (bit-and (.getShort bb (int p)) 0xFFFF)
                                    type (bit-and (.getShort bb (int (+ p 2))) 0xFFFF)
                                    cnt (.getInt bb (int (+ p 4)))]
                              :when (tag-names tag)]
                          [(tag-names tag) (read-values bb type cnt (+ p 8))]))
          curve (:tone-curve tags)
          p {:name (:name tags) :camera (:unique-camera-model tags)
             :illuminant-1 (some-> (:illuminant-1 tags) first) :illuminant-2 (some-> (:illuminant-2 tags) first)
             :color-matrix-1 (rows (:color-matrix-1 tags)) :color-matrix-2 (rows (:color-matrix-2 tags))
             :forward-matrix-1 (rows (:forward-matrix-1 tags)) :forward-matrix-2 (rows (:forward-matrix-2 tags))
             :hue-sat-1 (table (:hue-sat-dims tags) (:hue-sat-data-1 tags) (some-> (:hue-sat-encoding tags) first))
             :hue-sat-2 (table (:hue-sat-dims tags) (:hue-sat-data-2 tags) (some-> (:hue-sat-encoding tags) first))
             :look (table (:look-dims tags) (:look-data tags) (some-> (:look-encoding tags) first))
             :tone-curve (when (and curve (even? (alength ^floats curve)) (>= (alength ^floats curve) 4))
                           (vec (for [i (range (quot (alength ^floats curve) 2))]
                                  [(double (aget ^floats curve (* 2 i))) (double (aget ^floats curve (inc (* 2 i))))])))
             :baseline-exposure (double (or (some-> (:baseline-exposure tags) first) 0.0))}]
      (when-not (or (:color-matrix-1 p) (:forward-matrix-1 p))
        (throw (ex-info "The profile has no colour matrix" {})))
      p)))

(defn read-file
  "Parses the .dcp at `path`."
  [path]
  (parse (java.nio.file.Files/readAllBytes (.toPath (java.io.File. (str path))))))
