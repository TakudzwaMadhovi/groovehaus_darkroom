(ns darkroom.imaging.denoise
  "Offline, CPU-only noise reduction using OpenCV's non-local means
  (via the Bytedeco JavaCPP preset; no cloud API, no GPU runtime), run on
  16-bit data so RAW-grade gradients are not quantised to 8 bits first.
  Pure logic, no UI dependency."
  (:require [darkroom.imaging.core :as core]
            [darkroom.imaging.scene :as scene])
  (:import (org.bytedeco.javacpp ShortPointer)
           (org.bytedeco.opencv.global opencv_core opencv_photo)
           (org.bytedeco.opencv.opencv_core Mat)))

(set! *unchecked-math* :warn-on-boxed)

(defonce ^:private threads-configured
  ;; Leave one core free so the UI thread stays responsive while denoising.
  (delay (opencv_core/setNumThreads
           (int (max 1 (dec (.availableProcessors (Runtime/getRuntime))))))))

(def quality-params
  "NLM window sizes (both odd) per quality tier. Cost grows with the search
  window, quality with it too. :final is OpenCV's recommended 7/21 for
  full-resolution export."
  {:draft   {:template 5 :search 7}
   :preview {:template 5 :search 11}
   :final   {:template 7 :search 21}})

(def ^:private ^:const h-per-strength
  "Strength 0-100 -> NLM filter strength `h` in 16-bit units: 100 is h = 6000,
  about 9% of full scale. (OpenCV's 16-bit path only supports the L1 norm, whose
  `h` has a different scale from the 8-bit colour variant; this value was
  calibrated on noisy test images, see denoise-test.)"
  60.0)

(defn- ->mat16
  "Scene image -> 16-bit 3-channel Mat of the sRGB-encoded values (noise is
  most uniform there). Over-range and out-of-gamut values are clamped; the
  caller keeps the originals of those."
  ^Mat [{:keys [^long width ^long height data]}]
  (let [^floats src data
        ^shorts px (short-array (alength src))
        ^doubles enc (scene/encode-lut :srgb)]
    (core/parallel-ranges!
      (alength src)
      (fn [^long start ^long end]
        (loop [i start]
          (when (< i end)
            (aset px i (unchecked-short (Math/rint (* 65535.0 (scene/lut-at enc (aget src i))))))
            (recur (inc i))))))
    (let [m (Mat. (int height) (int width) opencv_core/CV_16UC3)]
      (.put (ShortPointer. (.data m)) px)
      m)))

(defn denoise
  "Reduces colour and luminance noise in a scene image. `strength` is 0 (off)
  to 100 (strongest). Options: :quality is :draft (fastest, while dragging a
  slider), :preview (default) or :final (slowest, for export). Channel values
  outside [0, 1] (headroom, out of gamut) are left as they were. Returns a new
  image; the input is not modified."
  [{:keys [width height data] :as img} strength & [{:keys [quality]}]]
  (if (<= (double strength) 0.0)
    img
    (do
      @threads-configured
      (let [{:keys [template search]} (quality-params (or quality :preview))
            h   (float (* h-per-strength (double strength)))
            src (->mat16 img)
            dst (Mat.)]
        (try
          (opencv_photo/fastNlMeansDenoising src dst (float-array [h h h]) (int template) (int search)
                                             opencv_core/NORM_L1)
          (let [^floats in data
                n (alength in)
                ^shorts den (short-array n)
                ^floats out (float-array n)
                ^doubles dlut scene/srgb-decode-lut]
            (.get (ShortPointer. (.data dst)) den)
            (core/parallel-ranges!
              n
              (fn [^long start ^long end]
                (loop [i start]
                  (when (< i end)
                    (let [v (double (aget in i))]
                      (aset out i (if (and (>= v 0.0) (<= v 1.0))
                                    (float (scene/lut-at dlut (/ (double (bit-and (aget den i) 0xFFFF)) 65535.0)))
                                    (float v))))
                    (recur (inc i))))))
            (scene/image width height out))
          (finally (.close src) (.close dst)))))))

(defn warm-up!
  "Loads OpenCV's native libraries and runs one tiny denoise so the first real
  slider move does not pay the ~1.5 s native-load cost. Safe to call from a
  background thread, repeatedly, and when OpenCV is unavailable (returns false)."
  []
  (try
    (denoise (scene/image 16 16 (float-array (* 3 256) 0.2)) 50 {:quality :draft})
    true
    (catch Throwable _ false)))
