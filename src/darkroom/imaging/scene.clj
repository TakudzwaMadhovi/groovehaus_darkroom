(ns darkroom.imaging.scene
  "Float, scene-linear images in the wide-gamut working space (see
  darkroom.imaging.color), plus the conversions to and from 8-bit display
  images. Editing happens here, not on 8-bit data, so exposure pushes and
  saturation boosts keep their precision and the extra gamut.

  A scene image is {:width w :height h :data floats}: interleaved R G B
  (3 floats per pixel), linear light, working-space primaries. Values may leave
  [0, 1] (headroom above white, small negatives out of gamut); they are only
  clamped when converted for display or export. Pure logic, no UI dependency."
  (:require [darkroom.imaging.color :as color]
            [darkroom.imaging.core :as core]))

(set! *unchecked-math* :warn-on-boxed)

(defn image
  "Builds a scene image from dimensions and an interleaved RGB float array."
  [width height ^floats data]
  {:width width :height height :data data})

(defn pixel-count ^long [{:keys [width height]}] (* (long width) (long height)))

;; ------------------------------------------------------------ lookup tables

(defn make-lut
  "Samples `f` (double -> double) at n+1 evenly spaced points on [0, 1]."
  ^doubles [f ^long n]
  (let [lut (double-array (inc n))]
    (dotimes [i (inc n)] (aset lut i (double (f (/ (double i) (double n))))))
    lut))

(defmacro lut-at
  "Linearly interpolated lookup of `x` (clamped to [0, 1]) in `lut`, a table
  made by make-lut."
  [lut x]
  `(let [^"[D" l# ~lut
         n#  (dec (alength l#))
         p#  (* (max 0.0 (min 1.0 (double ~x))) (double n#))
         i#  (min (dec n#) (long p#))
         f#  (- p# (double i#))
         a#  (aget l# i#)]
     (+ a# (* f# (- (aget l# (inc i#)) a#)))))

(def ^:const encode-steps 16384)
(def ^:const decode-steps 4096)

(defonce ^:private encode-luts (atom {}))

(defn encode-lut
  "Lookup table for linear -> encoded of the output space `space`."
  ^doubles [space]
  (or (@encode-luts space)
      (let [lut (make-lut (color/trc-encode-fn (:trc (color/spaces space))) encode-steps)]
        (swap! encode-luts assoc space lut)
        lut)))

(def ^"[D" srgb-decode-lut (make-lut color/srgb-decode decode-steps))

(defn srgb-encode-extended
  "sRGB encode that continues past 1.0 (and mirrors below 0) so over-range
  values survive the perceptual-domain edits. `x` is linear."
  ^double [^double x]
  (cond (< x 0.0) 0.0
        (<= x 1.0) (lut-at (encode-lut :srgb) x)
        :else (- (* 1.055 (Math/pow x (/ 1.0 2.4))) 0.055)))

(defn srgb-decode-extended
  "Inverse of srgb-encode-extended."
  ^double [^double v]
  (cond (< v 0.0) 0.0
        (<= v 1.0) (lut-at srgb-decode-lut v)
        :else (Math/pow (/ (+ v 0.055) 1.055) 2.4)))

;; ---------------------------------------------------------------- sources

(def ^:private srgb->working (color/convert-matrix :srgb :working))
(def ^:private working->srgb (color/convert-matrix :working :srgb))

(defn from-argb
  "8-bit sRGB display image (packed ARGB) -> scene image. Alpha is dropped."
  [{:keys [width height pixels]}]
  (let [^ints src pixels
        n   (alength src)
        ^floats out (float-array (* 3 n))
        ^doubles lin (let [d (double-array 256)]
                       (dotimes [i 256] (aset d i (color/srgb-decode (/ (double i) 255.0))))
                       d)
        ^doubles m (double-array srgb->working)
        m0 (aget m 0) m1 (aget m 1) m2 (aget m 2) m3 (aget m 3) m4 (aget m 4)
        m5 (aget m 5) m6 (aget m 6) m7 (aget m 7) m8 (aget m 8)]
    (core/parallel-ranges!
      n
      (fn [^long start ^long end]
        (loop [i start]
          (when (< i end)
            (let [p (aget src i)
                  r (aget lin (bit-and (unsigned-bit-shift-right p 16) 0xFF))
                  g (aget lin (bit-and (unsigned-bit-shift-right p 8) 0xFF))
                  b (aget lin (bit-and p 0xFF))
                  j (* 3 i)]
              (aset out j       (float (+ (* m0 r) (* m1 g) (* m2 b))))
              (aset out (+ j 1) (float (+ (* m3 r) (* m4 g) (* m5 b))))
              (aset out (+ j 2) (float (+ (* m6 r) (* m7 g) (* m8 b)))))
            (recur (inc i))))))
    (image width height out)))

(defn from-linear16
  "LibRaw's 16-bit linear output (decoded with output colour 4, i.e. already in
  the working space; see darkroom.imaging.raw) -> scene image, 65535 = 1.0."
  [{:keys [width height data]}]
  (let [^shorts src data
        n   (alength src)
        ^floats out (float-array n)
        k   (/ 1.0 65535.0)]
    (core/parallel-ranges!
      n
      (fn [^long start ^long end]
        (loop [i start]
          (when (< i end)
            (aset out i (float (* k (double (bit-and (aget src i) 0xFFFF)))))
            (recur (inc i))))))
    (image width height out)))

;; ----------------------------------------------------------------- outputs

(defn ->argb
  "Scene image -> packed-ARGB image in the output `space` (default :srgb):
  matrix to the space, clamp to its gamut, apply its transfer curve, quantise
  to 8 bits. Alpha is opaque."
  ([scene] (->argb scene :srgb))
  ([{:keys [width height data]} space]
   (let [^floats src data
         n   (quot (alength src) 3)
         ^ints out (int-array n)
         ^doubles m (double-array (color/convert-matrix :working space))
         m0 (aget m 0) m1 (aget m 1) m2 (aget m 2) m3 (aget m 3) m4 (aget m 4)
         m5 (aget m 5) m6 (aget m 6) m7 (aget m 7) m8 (aget m 8)
         ^doubles enc (encode-lut space)]
     (core/parallel-ranges!
       n
       (fn [^long start ^long end]
         (loop [i start]
           (when (< i end)
             (let [j (* 3 i)
                   r (double (aget src j)) g (double (aget src (+ j 1))) b (double (aget src (+ j 2)))
                   lr (+ (* m0 r) (* m1 g) (* m2 b))
                   lg (+ (* m3 r) (* m4 g) (* m5 b))
                   lb (+ (* m6 r) (* m7 g) (* m8 b))
                   er (long (Math/rint (* 255.0 (lut-at enc lr))))
                   eg (long (Math/rint (* 255.0 (lut-at enc lg))))
                   eb (long (Math/rint (* 255.0 (lut-at enc lb))))]
               (aset out i (unchecked-int (bit-or 0xFF000000 (bit-shift-left er 16) (bit-shift-left eg 8) eb))))
             (recur (inc i))))))
     (core/image width height out))))

(defn ->encoded16
  "Scene image -> interleaved RGB `shorts` (unsigned 16-bit) in the output
  `space`, for 16-bit TIFF export. Returns {:width :height :data}."
  [{:keys [width height data]} space]
  (let [^floats src data
        n   (quot (alength src) 3)
        ^shorts out (short-array (* 3 n))
        ^doubles m (double-array (color/convert-matrix :working space))
        m0 (aget m 0) m1 (aget m 1) m2 (aget m 2) m3 (aget m 3) m4 (aget m 4)
        m5 (aget m 5) m6 (aget m 6) m7 (aget m 7) m8 (aget m 8)
        ^doubles enc (encode-lut space)]
    (core/parallel-ranges!
      n
      (fn [^long start ^long end]
        (loop [i start]
          (when (< i end)
            (let [j (* 3 i)
                  r (double (aget src j)) g (double (aget src (+ j 1))) b (double (aget src (+ j 2)))]
              (aset out j       (unchecked-short (Math/rint (* 65535.0 (lut-at enc (+ (* m0 r) (* m1 g) (* m2 b)))))))
              (aset out (+ j 1) (unchecked-short (Math/rint (* 65535.0 (lut-at enc (+ (* m3 r) (* m4 g) (* m5 b)))))))
              (aset out (+ j 2) (unchecked-short (Math/rint (* 65535.0 (lut-at enc (+ (* m6 r) (* m7 g) (* m8 b))))))))
            (recur (inc i))))))
    {:width width :height height :data out}))

;; ------------------------------------------------------------------ resize

(defn fit
  "Returns `scene` shrunk by an integer box average (in linear light) so its
  longest side is at most `max-side`; `scene` itself if it already fits."
  [{:keys [^long width ^long height data] :as scene} max-side]
  (let [k (long (Math/ceil (/ (double (max width height)) (double max-side))))]
    (if (<= k 1)
      scene
      (let [^floats src data
            w (quot width k) h (quot height k)
            ^floats out (float-array (* 3 w h))
            kk  (* k k)
            inv (/ 1.0 (double kk))]
        (core/parallel-ranges!
          h
          (fn [^long y0 ^long y1]
            (loop [y y0]
              (when (< y y1)
                (dotimes [x w]
                  (let [sx (* x k) sy (* y k)]
                    (dotimes [c 3]
                      (let [acc (double (loop [t 0 acc 0.0]
                                  (if (< t kk)
                                    (let [j (quot t k) i (rem t k)]
                                      (recur (inc t)
                                             (+ acc (double (aget src (+ (* 3 (+ (* (+ sy j) width) sx i)) c))))))
                                    acc)))]
                        (aset out (+ (* 3 (+ (* y w) x)) c) (float (* acc inv)))))))
                (recur (inc y))))))
        (image w h out)))))
