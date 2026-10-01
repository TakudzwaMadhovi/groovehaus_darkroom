(ns darkroom.imaging.denoise
  "Offline, CPU-only noise reduction using OpenCV's non-local means
  (via the Bytedeco JavaCPP preset; no cloud API, no GPU runtime).
  Pure logic, no UI dependency."
  (:import (org.bytedeco.opencv.global opencv_core opencv_photo)
           (org.bytedeco.opencv.opencv_core Mat)))

(set! *unchecked-math* :warn-on-boxed)

(defonce ^:private threads-configured
  ;; Leave one core free so the UI thread stays responsive while denoising.
  (delay (opencv_core/setNumThreads
           (int (max 1 (dec (.availableProcessors (Runtime/getRuntime))))))))

(def quality-params
  "NLM window sizes (both odd) per quality tier. Cost grows with the search
  window, quality with it too. Measured on a 1600x1067 noisy image, 4 cores:
  :draft ~320 ms / 29.9 dB PSNR, :preview ~620 ms / 31.9 dB, :final is OpenCV's
  recommended 7/21 for full-resolution export."
  {:draft   {:template 5 :search 7}
   :preview {:template 5 :search 11}
   :final   {:template 7 :search 21}})

(defn- ->mat
  "Image map -> 8-bit BGR Mat (alpha is dropped; see `denoise`)."
  ^Mat [{:keys [^long width ^long height pixels]}]
  (let [^ints src pixels
        n (alength src)
        ^bytes bgr (byte-array (* 3 n))]
    (dotimes [i n]
      (let [p (aget src i) j (* 3 i)]
        (aset bgr j       (unchecked-byte p))
        (aset bgr (+ j 1) (unchecked-byte (unsigned-bit-shift-right p 8)))
        (aset bgr (+ j 2) (unchecked-byte (unsigned-bit-shift-right p 16)))))
    (let [m (Mat. (int height) (int width) opencv_core/CV_8UC3)]
      (.put (.data m) bgr)
      m)))

(defn denoise
  "Reduces colour and luminance noise. `strength` is 0 (off) to 100 (strongest).
  Options: :quality is :draft (fastest, while dragging a slider), :preview
  (default) or :final (slowest, for export). Alpha is preserved.
  Returns a new image map; the input is not modified."
  [{:keys [width height pixels] :as img} strength & [{:keys [quality]}]]
  (if (<= (double strength) 0.0)
    img
    (do
      @threads-configured
      (let [{:keys [template search]} (quality-params (or quality :preview))
            ;; Strength 0-100 -> NLM `h` 0-15 (OpenCV suggests ~3-10 for typical noise).
            h   (float (* 0.15 (double strength)))
            src (->mat img)
            dst (Mat.)]
        (try
          (opencv_photo/fastNlMeansDenoisingColored src dst h h (int template) (int search))
          (let [^ints in pixels
                n (alength in)
                ^bytes bgr (byte-array (* 3 n))
                ^ints out (int-array n)]
            (.get (.data dst) bgr)
            (dotimes [i n]
              (let [j (* 3 i)
                    b (bit-and (aget bgr j) 0xFF)
                    g (bit-and (aget bgr (+ j 1)) 0xFF)
                    r (bit-and (aget bgr (+ j 2)) 0xFF)]
                (aset out i (unchecked-int (bit-or (bit-and (aget in i) 0xFF000000)
                                                   (bit-shift-left r 16) (bit-shift-left g 8) b)))))
            {:width width :height height :pixels out})
          (finally (.close src) (.close dst)))))))

(defn warm-up!
  "Loads OpenCV's native libraries and runs one tiny denoise so the first real
  slider move does not pay the ~1.5 s native-load cost. Safe to call from a
  background thread, repeatedly, and when OpenCV is unavailable (returns false)."
  []
  (try
    (denoise {:width 16 :height 16 :pixels (int-array 256 (unchecked-int 0xFF808080))} 50 {:quality :draft})
    true
    (catch Throwable _ false)))
