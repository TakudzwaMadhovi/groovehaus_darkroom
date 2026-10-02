(ns darkroom.dcp-fixture
  "Test helper: builds the bytes of a small DNG camera profile (.dcp)."
  (:import (java.nio ByteBuffer ByteOrder)))

(defn- field-bytes ^bytes [type vals]
  (let [vals (if (string? vals) (vec (map int (str vals "\u0000"))) vals)
        size ({1 1 2 1 3 2 4 4 10 8 11 4} type)
        bb (doto (ByteBuffer/allocate (* size (if (= type 10) (count vals) (count vals)))) (.order ByteOrder/LITTLE_ENDIAN))]
    (doseq [v vals]
      (case (int type)
        (1 2) (.put bb (unchecked-byte v))
        3 (.putShort bb (unchecked-short v))
        4 (.putInt bb (unchecked-int v))
        10 (do (.putInt bb (int (Math/round (* (double v) 100000.0)))) (.putInt bb (int 100000)))
        11 (.putFloat bb (float v))))
    (.array bb)))

(defn- count-of [type vals] (if (string? vals) (inc (count vals)) (count vals)))

(defn dcp-bytes
  "`entries` is a seq of [tag type values]; types 2 ascii, 3 short, 4 long,
  10 srational (values are doubles), 11 float."
  ^bytes [entries]
  (let [entries (sort-by first entries)
        n (count entries)
        ifd-off 8
        data-start (+ ifd-off 2 (* 12 n) 4)
        fields (map (fn [[t ty v]] [t ty (count-of ty v) (field-bytes ty v)]) entries)
        offsets (loop [fs fields pos data-start acc []]
                  (if (empty? fs) acc
                      (let [[_ _ _ ^bytes bs] (first fs)]
                        (if (<= (alength bs) 4)
                          (recur (rest fs) pos (conj acc nil))
                          (recur (rest fs) (+ pos (alength bs) (mod (alength bs) 2)) (conj acc pos))))))
        total (+ data-start (reduce + (for [[_ _ _ ^bytes bs] fields :when (> (alength bs) 4)] (+ (alength bs) (mod (alength bs) 2)))))
        bb (doto (ByteBuffer/allocate total) (.order ByteOrder/LITTLE_ENDIAN))]
    (.put bb (.getBytes "IIRC" "US-ASCII")) (.putInt bb ifd-off)
    (.putShort bb (short n))
    (doseq [[[t ty c ^bytes bs] off] (map vector fields offsets)]
      (.putShort bb (unchecked-short t)) (.putShort bb (unchecked-short ty)) (.putInt bb (int c))
      (if off
        (.putInt bb (int off))
        (let [p (byte-array 4)] (System/arraycopy bs 0 p 0 (alength bs)) (.put bb p))))
    (.putInt bb 0)
    (doseq [[[_ _ _ ^bytes bs] off] (map vector fields offsets) :when off]
      (.position bb (int off)) (.put bb bs))
    (.array bb)))

(def srgb-from-xyz-d65
  [3.2406 -1.5372 -0.4986 -0.9689 1.8758 0.0415 0.0557 -0.2040 1.0570])

(defn table-floats
  "A hue/sat/val table of (shift scale scale) entries from (f h s v) -> [shift sat val]."
  [H S V f]
  (vec (mapcat (fn [[v h s]] (f h s v))
               (for [v (range V) h (range H) s (range S)] [v h s]))))
