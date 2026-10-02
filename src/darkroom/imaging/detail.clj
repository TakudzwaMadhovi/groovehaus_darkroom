(ns darkroom.imaging.detail
  "Local contrast, sharpening, dehaze and colour-noise reduction on scene
  images. Pure logic (OpenCV only for blurs and filters), no UI dependency.

  Radii that describe a feature of the picture (texture, clarity, dehaze) are
  relative to the long edge, so a downscaled preview looks like the export.
  Radii that describe pixels (sharpening, colour noise) are in pixels of the
  full-size image and are multiplied by the `:scale` option (preview width /
  source width) so the preview shows the same thing proportionally."
  (:require [darkroom.imaging.color :as color]
            [darkroom.imaging.core :as core]
            [darkroom.imaging.scene :as scene])
  (:import (org.bytedeco.javacpp FloatPointer)
           (org.bytedeco.opencv.global opencv_core opencv_imgproc)
           (org.bytedeco.opencv.opencv_core Mat Size)))

(set! *unchecked-math* :warn-on-boxed)

(def defaults
  {:texture 0.0 :clarity 0.0 :dehaze 0.0
   :sharpen 0.0 :sharpen-radius 1.0 :sharpen-masking 0.0
   :denoise-color 0.0})

(def detail-keys [:texture :clarity :sharpen :sharpen-radius :sharpen-masking])
(def dehaze-keys [:dehaze])
(def color-nr-keys [:denoise-color])

(defn- setting ^double [settings k] (double (get settings k (defaults k))))

(defn detail-neutral? [settings]
  (and (zero? (setting settings :texture)) (zero? (setting settings :clarity)) (zero? (setting settings :sharpen))))

(defn dehaze-neutral? [settings] (zero? (setting settings :dehaze)))

(defn color-nr-neutral? [settings] (zero? (setting settings :denoise-color)))

;; ----------------------------------------------------------------- planes

(defn- ->mat
  "float plane -> single-channel 32-bit Mat."
  ^Mat [^floats a ^long w ^long h]
  (let [m (Mat. (int h) (int w) opencv_core/CV_32FC1)]
    (.put (FloatPointer. (.data m)) a)
    m))

(defn- mat->floats ^floats [^Mat m ^long n]
  (let [out (float-array n)]
    (.get (FloatPointer. (.data m)) out)
    out))

(defn blur-plane
  "Gaussian blur of the w x h float plane with standard deviation `sigma`
  (pixels). Large blurs are done on a reduced copy (they only keep low
  frequencies anyway), so cost stays flat as the radius grows. Returns a new
  array; `plane` itself when sigma is negligible."
  ^floats [^floats plane ^long w ^long h ^double sigma]
  (if (< sigma 0.3)
    plane
    (let [f   (long (Math/floor (/ sigma 4.0)))
          ^Mat src (->mat plane w h)
          ^Mat dst (Mat.)]
      (try
        (if (<= f 1)
          (opencv_imgproc/GaussianBlur src dst (Size. 0 0) sigma)
          (let [sw (max 1 (quot w f)) sh (max 1 (quot h f))
                small (Mat.) blurred (Mat.)]
            (try
              (opencv_imgproc/resize src small (Size. (int sw) (int sh)) 0.0 0.0 opencv_imgproc/INTER_AREA)
              (opencv_imgproc/GaussianBlur small blurred (Size. 0 0) (/ sigma (double f)))
              (opencv_imgproc/resize blurred dst (Size. (int w) (int h)) 0.0 0.0 opencv_imgproc/INTER_LINEAR)
              (finally (.close small) (.close blurred)))))
        (mat->floats dst (* w h))
        (finally (.close src) (.close dst))))))

(defn- luma-plane
  "Encoded (perceptual) luma of a scene image as a float plane."
  ^floats [{:keys [^long width ^long height data]}]
  (let [^floats src data
        n   (* width height)
        ^floats out (float-array n)
        ^doubles lw (double-array (color/luma-weights :working))
        lr (aget lw 0) lg (aget lw 1) lb (aget lw 2)]
    (core/parallel-ranges!
      n
      (fn [^long start ^long end]
        (loop [i start]
          (when (< i end)
            (let [j (* 3 i)]
              (aset out i (float (scene/srgb-encode-extended
                                   (+ (* lr (double (aget src j))) (* lg (double (aget src (+ j 1)))) (* lb (double (aget src (+ j 2)))))))))
            (recur (inc i))))))
    out))

(defn- add-luma-delta
  "New scene image: each pixel's encoded channels shifted by `delta` (a plane),
  so brightness changes without touching the colour differences."
  [{:keys [^long width ^long height data]} ^floats delta]
  (let [^floats src data
        n (* width height)
        ^floats out (float-array (alength src))]
    (core/parallel-ranges!
      n
      (fn [^long start ^long end]
        (loop [i start]
          (when (< i end)
            (let [d (double (aget delta i)) j (* 3 i)]
              (dotimes [c 3]
                (aset out (+ j c) (float (scene/srgb-decode-extended
                                           (+ d (scene/srgb-encode-extended (double (aget src (+ j c))))))))))
            (recur (inc i))))))
    (scene/image width height out)))

(defn- smoothstep ^double [^double x]
  (let [t (max 0.0 (min 1.0 x))] (* t t (- 3.0 (* 2.0 t)))))

;; --------------------------------------------- texture / clarity / sharpen

(def ^:private ^:const texture-sigma 0.0015) ; fractions of the long edge
(def ^:private ^:const clarity-sigma 0.012)

(defn local-contrast
  "Texture, clarity and sharpening on luminance only (colours are untouched).
  Settings: :texture and :clarity (-1..1), :sharpen (0..100), :sharpen-radius
  (px at full size, 0.5-3), :sharpen-masking (0..1: 0 sharpens everything,
  1 only real edges). opts :scale is preview width / source width (default 1)."
  [{:keys [^long width ^long height] :as img} settings & [opts]]
  (if (detail-neutral? settings)
    img
    (let [scale   (double (or (:scale opts) 1.0))
          long-e  (double (max width height))
          tex     (setting settings :texture)
          clar    (setting settings :clarity)
          amount  (* 0.016 (setting settings :sharpen))
          masking (setting settings :sharpen-masking)
          radius  (max 0.5 (* scale (setting settings :sharpen-radius)))
          n       (* width height)
          ^floats l (luma-plane img)
          ^floats bt (when-not (zero? tex) (blur-plane l width height (max 1.0 (* texture-sigma long-e))))
          ^floats bc (when-not (zero? clar) (blur-plane l width height (* clarity-sigma long-e)))
          ^floats bs (when-not (zero? amount) (blur-plane l width height radius))
          ^floats delta (float-array n)]
      (core/parallel-ranges!
        n
        (fn [^long start ^long end]
          (loop [i start]
            (when (< i end)
              (let [lv (double (aget l i))
                    dt (if bt (* tex (- lv (double (aget bt i)))) 0.0)
                    dc (if bc (* clar 0.8 (max 0.0 (min 1.0 (* 4.0 lv (- 1.0 lv)))) (- lv (double (aget bc i)))) 0.0)
                    ds (if bs
                         (let [hf (- lv (double (aget bs i)))
                               edge (smoothstep (* 12.0 (Math/abs hf)))
                               w (+ (- 1.0 masking) (* masking edge))]
                           (max -0.25 (min 0.25 (* amount w hf))))
                         0.0)]
                (aset delta i (float (+ dt dc ds))))
              (recur (inc i))))))
      (add-luma-delta img delta))))

;; -------------------------------------------------------------- colour NR

(defn reduce-color-noise
  "Smooths chroma noise (blotchy colour speckle) while keeping luminance and
  real colour edges: the two colour-difference planes go through an
  edge-preserving bilateral filter. `:denoise-color` is 0-100."
  [{:keys [^long width ^long height data] :as img} settings & [opts]]
  (if (color-nr-neutral? settings)
    img
    (let [a      (/ (setting settings :denoise-color) 100.0)
          scale  (double (or (:scale opts) 1.0))
          sigma  (max 1.0 (* a 8.0 scale))
          sigc   (+ 0.05 (* 0.15 a))
          d      (int (max 5 (min 25 (inc (* 2 (long (Math/ceil (* 1.5 sigma))))))))
          n      (* width height)
          ^floats src data
          ^doubles lw (double-array (color/luma-weights :working))
          lr (aget lw 0) lg (aget lw 1) lb (aget lw 2)
          ^floats ep (float-array (* 3 n))   ; encoded channels
          ^floats cr (float-array n)
          ^floats cb (float-array n)
          ^floats ly (float-array n)]
      (core/parallel-ranges!
        n
        (fn [^long start ^long end]
          (loop [i start]
            (when (< i end)
              (let [j (* 3 i)
                    r (scene/srgb-encode-extended (double (aget src j)))
                    g (scene/srgb-encode-extended (double (aget src (+ j 1))))
                    b (scene/srgb-encode-extended (double (aget src (+ j 2))))
                    y (+ (* lr r) (* lg g) (* lb b))]
                (aset ly i (float y))
                (aset cr i (float (- r y)))
                (aset cb i (float (- b y))))
              (recur (inc i))))))
      (let [filt (fn ^floats [^floats plane]
                   (let [^Mat m (->mat plane width height) ^Mat o (Mat.)]
                     (try (opencv_imgproc/bilateralFilter m o d sigc sigma)
                          (mat->floats o n)
                          (finally (.close m) (.close o)))))
            ^floats fcr (filt cr)
            ^floats fcb (filt cb)
            ^floats out (float-array (* 3 n))]
        (core/parallel-ranges!
          n
          (fn [^long start ^long end]
            (loop [i start]
              (when (< i end)
                (let [j (* 3 i)
                      y (double (aget ly i))
                      r (+ y (double (aget fcr i)))
                      b (+ y (double (aget fcb i)))
                      g (/ (- y (* lr r) (* lb b)) lg)]
                  (aset out j       (float (scene/srgb-decode-extended r)))
                  (aset out (+ j 1) (float (scene/srgb-decode-extended g)))
                  (aset out (+ j 2) (float (scene/srgb-decode-extended b))))
                (recur (inc i))))))
        (scene/image width height out)))))

;; ------------------------------------------------------------------ dehaze

(def ^:private haze-side 600)

(defn- resize-plane
  "Bilinear resize of a w x h float plane to nw x nh."
  ^floats [^floats plane w h nw nh]
  (let [^Mat m (->mat plane w h) ^Mat o (Mat.)]
    (try (opencv_imgproc/resize m o (Size. (int nw) (int nh)) 0.0 0.0 opencv_imgproc/INTER_LINEAR)
         (mat->floats o (* (long nw) (long nh)))
         (finally (.close m) (.close o)))))

(defn- dark-channel
  "Per-pixel min of the (clamped) channels, eroded over a window: the haze
  estimate of He et al.'s dark channel prior. Float plane."
  ^floats [{:keys [^long width ^long height data]}]
  (let [^floats src data
        n (* width height)
        ^floats d (float-array n)]
    (dotimes [i n]
      (let [j (* 3 i)]
        (aset d i (float (max 0.0 (min 1.0 (min (double (aget src j)) (min (double (aget src (+ j 1))) (double (aget src (+ j 2)))))))))))
    (let [^Mat m (->mat d width height) ^Mat o (Mat.)
          ^Mat kernel (opencv_imgproc/getStructuringElement opencv_imgproc/MORPH_RECT (Size. 15 15))]
      (try (opencv_imgproc/erode m o kernel)
           (mat->floats o n)
           (finally (.close m) (.close o) (.close kernel))))))

(defn dehaze
  "Removes (+) or adds (-) atmospheric haze, `:dehaze` -1..1. Removal estimates
  the haze from the dark channel on a reduced copy, then inverts the haze
  model I = J t + A (1 - t) in linear light."
  [{:keys [^long width ^long height data] :as img} settings]
  (if (dehaze-neutral? settings)
    img
    (let [amount (setting settings :dehaze)
          ^floats src data
          n (* width height)
          ^floats out (float-array (alength src))]
      (if (neg? amount)
        ;; add haze: blend toward a flat veil
        (let [t (- 1.0 (* 0.5 (- amount))) veil (* 0.8 (- 1.0 t))]
          (dotimes [i (alength src)] (aset out i (float (+ (* t (double (aget src i))) veil)))))
        (let [small (scene/fit img haze-side)
              sw (long (:width small)) sh (long (:height small))
              ^floats dk (blur-plane (dark-channel small) sw sh 3.0)
              m  (* sw sh)
              ;; airlight: mean colour of the haziest 0.1% (at least 1 pixel)
              cnt   (max 1 (quot m 1000))
              thr   (let [sorted (float-array dk)] (java.util.Arrays/sort sorted) (aget sorted (- m cnt)))
              order (vec (take cnt (filter #(>= (aget dk (int %)) thr) (range m))))
              ^floats ssrc (:data small)
              air (vec (for [c (range 3)]
                         (max 0.3 (/ (double (reduce + (map #(double (aget ssrc (+ (* 3 (int %)) (long c)))) order)))
                                     (double (count order))))))
              a-mean (/ (double (reduce + air)) 3.0)
              ^floats ts (let [t (float-array m)]
                           (dotimes [i m]
                             (aset t i (float (max 0.2 (- 1.0 (* 0.95 amount (/ (double (aget dk i)) a-mean)))))))
                           t)
              ^floats tf (resize-plane ts sw sh width height)
              ^doubles airs (double-array air)]
          (core/parallel-ranges!
            n
            (fn [^long start ^long end]
              (loop [i start]
                (when (< i end)
                  (let [t (max 0.2 (double (aget tf i))) j (* 3 i)]
                    (dotimes [c 3]
                      (let [a (aget airs c)]
                        (aset out (+ j c) (float (+ a (/ (- (double (aget src (+ j c))) a) t)))))))
                  (recur (inc i))))))))
      (scene/image width height out))))
