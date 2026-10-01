(ns darkroom.dng
  "Test helper: builds a tiny synthetic Bayer DNG on the fly (no binary
  fixtures). Optionally embeds a JPEG preview in IFD0 (as real DNGs/ARWs do,
  with the raw data in a SubIFD) and an Orientation tag."
  (:import (java.io File)
           (java.nio ByteBuffer ByteOrder)))

(def ^:private srgb-from-xyz ; DNG ColorMatrix1: XYZ(D65) -> camera, camera == linear sRGB
  [[3.2406 -1.5372 -0.4986] [-0.9689 1.8758 0.0415] [0.0557 -0.2040 1.0570]])

(def ^:private type-size {1 1, 2 1, 3 2, 4 4, 5 4, 10 4})

(defn- encode
  "Value vector -> bytes (little-endian) for a TIFF field type."
  ^bytes [type vals]
  (let [vals (if (string? vals) (map int (str vals "\u0000")) vals)
        bb (doto (ByteBuffer/allocate (* (count vals) (type-size type))) (.order ByteOrder/LITTLE_ENDIAN))]
    (doseq [v vals]
      (case (int type)
        (1 2) (.put bb (unchecked-byte v))
        3     (.putShort bb (unchecked-short v))
        (4 5 10) (.putInt bb (unchecked-int v))))
    (.array bb)))

(defn- count-of [type vals]
  (let [n (if (string? vals) (inc (count vals)) (count vals))]
    (if (#{5 10} type) (quot n 2) n)))

(defn dng-bytes
  "opts: :thumb JPEG bytes, :thumb-size [w h], :orientation EXIF value."
  [w h value {:keys [thumb thumb-size orientation]}]
  (let [raw-entries  [[254 4 [0]] [256 4 [w]] [257 4 [h]] [258 3 [16]] [259 3 [1]] [262 3 [32803]]
                      :strip-offset [277 3 [1]] [278 4 [h]] [279 4 [(* w h 2)]]
                      [33421 3 [2 2]] [33422 1 [0 1 1 2]] [50717 4 [65535]]]
        meta-entries [[271 2 "Test"] [272 2 "Synthetic"] [50706 1 [1 4 0 0]]
                      [50721 10 (vec (for [row srgb-from-xyz v row] [(Math/round (* (double v) 10000.0)) 10000]))]
                      [50728 5 [1 1 1 1 1 1]]]
        meta-entries (mapv (fn [[t ty v]] [t ty (if (vector? (first v)) (vec (mapcat identity v)) v)]) meta-entries)
        ;; With a thumbnail, IFD0 holds it plus SubIFDs -> raw IFD; otherwise IFD0 is the raw IFD.
        ifd0-n (if thumb (+ 12 (count meta-entries) (if orientation 1 0)) (+ (dec (count raw-entries)) 1 (count meta-entries) (if orientation 1 0)))
        ifd1-n (dec (count raw-entries))
        ifd-len #(+ 2 (* 12 %) 4)
        ifd0-off 8
        ifd1-off (+ ifd0-off (ifd-len ifd0-n))
        extra0 (+ ifd1-off (if thumb (ifd-len ifd1-n) 0))
        extra (atom [])      ; [offset bytes]
        pos   (atom extra0)
        alloc! (fn [^bytes bs] (let [o @pos] (swap! extra conj [o bs]) (swap! pos + (+ (alength bs) (mod (alength bs) 2))) o))
        raw-off (alloc! (byte-array (* w h 2)))
        thumb-off (when thumb (alloc! thumb))
        ->field (fn [ifd [t ty vals]]
                  (let [bs (encode ty vals) cnt (count-of ty vals)]
                    [t ty cnt (if (<= (alength bs) 4) (let [p (byte-array 4)] (System/arraycopy bs 0 p 0 (alength bs)) p) (alloc! bs))]))
        raw-fields (mapv #(->field 1 (if (= % :strip-offset) [273 4 [raw-off]] %)) raw-entries)
        thumb-fields (when thumb
                       (let [[tw th] thumb-size]
                         (mapv #(->field 0 %)
                               [[254 4 [1]] [256 4 [tw]] [257 4 [th]] [258 3 [8 8 8]] [259 3 [7]] [262 3 [6]]
                                [273 4 [thumb-off]] [277 3 [3]] [278 4 [th]] [279 4 [(alength ^bytes thumb)]]
                                [330 4 [ifd1-off]]])))
        orient-field (when orientation [(->field 0 [274 3 [orientation]])])
        meta-fields (mapv #(->field 0 %) meta-entries)
        bb (doto (ByteBuffer/allocate (* (count vals) (type-size type))) (.order ByteOrder/LITTLE_ENDIAN))]
    (doseq [v vals]
      (case (int type)
        (1 2) (.put bb (unchecked-byte v))
        3     (.putShort bb (unchecked-short v))
        (4 5 10) (.putInt bb (unchecked-int v))))
    (.array bb)))

(defn- count-of [type vals]
  (let [n (if (string? vals) (inc (count vals)) (count vals))]
    (if (#{5 10} type) (quot n 2) n)))

(defn dng-bytes
  "opts: :thumb JPEG bytes, :thumb-size [w h], :orientation EXIF value."
  [w h value {:keys [thumb thumb-size orientation]}]
  (let [raw-entries  [[254 4 [0]] [256 4 [w]] [257 4 [h]] [258 3 [16]] [259 3 [1]] [262 3 [32803]]
                      :strip-offset [277 3 [1]] [278 4 [h]] [279 4 [(* w h 2)]]
                      [33421 3 [2 2]] [33422 1 [0 1 1 2]] [50717 4 [65535]]]
        meta-entries [[271 2 "Test"] [272 2 "Synthetic"] [50706 1 [1 4 0 0]]
                      [50721 10 (vec (for [row srgb-from-xyz v row] [(Math/round (* (double v) 10000.0)) 10000]))]
                      [50728 5 [1 1 1 1 1 1]]]
        meta-entries (mapv (fn [[t ty v]] [t ty (if (vector? (first v)) (vec (mapcat identity v)) v)]) meta-entries)
        ;; With a thumbnail, IFD0 holds it plus SubIFDs -> raw IFD; otherwise IFD0 is the raw IFD.
        ifd0-n (if thumb (+ 12 (count meta-entries) (if orientation 1 0)) (+ (dec (count raw-entries)) 1 (count meta-entries) (if orientation 1 0)))
        ifd1-n (dec (count raw-entries))
        ifd-len #(+ 2 (* 12 %) 4)
        ifd0-off 8
        ifd1-off (+ ifd0-off (ifd-len ifd0-n))
        extra0 (+ ifd1-off (if thumb (ifd-len ifd1-n) 0))
        extra (atom [])      ; [offset bytes]
        pos   (atom extra0)
        alloc! (fn [^bytes bs] (let [o @pos] (swap! extra conj [o bs]) (swap! pos + (+ (alength bs) (mod (alength bs) 2))) o))
        raw-off (alloc! (byte-array (* w h 2)))
        thumb-off (when thumb (alloc! thumb))
        ->field (fn [ifd [t ty vals]]
                  (let [bs (encode ty vals) cnt (count-of ty vals)]
                    [t ty cnt (if (<= (alength bs) 4) (let [p (byte-array 4)] (System/arraycopy bs 0 p 0 (alength bs)) p) (alloc! bs))]))
        raw-fields (mapv #(->field 1 (if (= % :strip-offset) [273 4 [raw-off]] %)) raw-entries)
        thumb-fields (when thumb
                       (let [[tw th] thumb-size]
                         (mapv #(->field 0 %)
                               [[254 4 [1]] [256 4 [tw]] [257 4 [th]] [258 3 [8 8 8]] [259 3 [7]] [262 3 [6]]
                                [273 4 [thumb-off]] [277 3 [3]] [278 4 [th]] [279 4 [(alength ^bytes thumb)]]
                                [330 4 [ifd1-off]]])))
        orient-field (when orientation [(->field 0 [274 3 [orientation]])])
        meta-fields (mapv #(->field 0 %) meta-entries)
        ifd0-fields (sort-by first (concat (if thumb thumb-fields (remove #(= 254 (first %)) raw-fields)) orient-field meta-fields
                                           (when-not thumb [(->field 0 [254 4 [0]])])))
        ifd0-fields (if thumb ifd0-fields (sort-by first (concat (filter #(not= 254 (first %)) ifd0-fields) [])))
        bb (doto (ByteBuffer/allocate (+ @pos 8)) (.order ByteOrder/LITTLE_ENDIAN))
        write-ifd (fn [off fields]
                    (.position bb (int off))
                    (.putShort bb (short (count fields)))
                    (doseq [[t ty cnt v] (sort-by first fields)]
                      (.putShort bb (unchecked-short t)) (.putShort bb (unchecked-short ty)) (.putInt bb (int cnt))
                      (if (bytes? v) (.put bb ^bytes v) (.putInt bb (int v))))
                    (.putInt bb 0))]
    (.put bb (.getBytes "II")) (.putShort bb (short 42)) (.putInt bb ifd0-off)
    (write-ifd ifd0-off (if thumb (concat thumb-fields orient-field meta-fields) (concat raw-fields orient-field meta-fields)))
    (when thumb (write-ifd ifd1-off raw-fields))
    (doseq [[o ^bytes bs] @extra] (.position bb (int o)) (.put bb bs))
    ;; sample data: every photosite = value
    (let [sb (.asShortBuffer (doto (.duplicate bb) (.position (int raw-off)) (.order ByteOrder/LITTLE_ENDIAN)))]
      (dotimes [_ (* w h)] (.put sb (unchecked-short value))))
    (.array bb)))

(defn write-dng!
  (^File [w h value] (write-dng! w h value {}))
  (^File [w h value opts]
   (let [f (File/createTempFile "darkroom-" ".dng")]
     (.deleteOnExit f)
     (with-open [o (java.io.FileOutputStream. f)] (.write o ^bytes (dng-bytes w h value opts)))
     f)))
