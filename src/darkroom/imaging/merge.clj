(ns darkroom.imaging.merge
  "Combining several frames into one scene image: HDR merge of an exposure
  bracket (this namespace) and panoramas (darkroom.imaging.panorama).

  The HDR merge works in linear light on the editor's float scene images, so a
  bracket of RAW files merges without any tone curve to recover: each pixel is
  the average of the frames' values divided by their relative exposure, weighted
  so that clipped or very dark samples count for nothing and longer exposures
  (less noise) count for more. Over-range results (above 1.0) are kept; the
  result is in the units of the reference (middle) exposure. JPEG brackets work
  too but their tone curve is only approximately undone by the sRGB decode.
  Pure logic (OpenCV only for the alignment), no UI dependency."
  (:require [darkroom.imaging.core :as core]
            [darkroom.imaging.scene :as scene])
  (:import (org.bytedeco.javacpp FloatPointer)
           (org.bytedeco.opencv.global opencv_core opencv_imgproc)
           (org.bytedeco.opencv.opencv_core Mat Point2d Size)))


;; --------------------------------------------------------- exposure factors

(defn exposure-value
  "Relative amount of light a frame's settings let in: shutter time x ISO /
  f-number^2 (rationals as [n d] pairs, as exif/read-tags returns them), or nil
  when the tags are incomplete."
  [{:keys [exposure-time f-number iso]}]
  (let [r (fn [[n d]] (when (and n d (pos? d)) (/ (double n) (double d))))
        t (when exposure-time (r exposure-time)) f (when f-number (r f-number))]
    (when (and t f iso (pos? (double iso)))
      (/ (* t (double iso)) (* f f)))))

(defn- luma-sample
  "Luminance-like value (the channel mean) of pixel i of float RGB `d`."
  ^double [^floats d ^long i]
  (/ (+ (aget d (* 3 i)) (aget d (+ (* 3 i) 1)) (aget d (+ (* 3 i) 2))) 3.0))

(defn- mean-brightness ^double [{:keys [^long width ^long height data]}]
  (let [^floats d data n (* width height) step (max 1 (quot n 20000))]
    (loop [i 0 acc 0.0 c 0]
      (if (< i n) (recur (+ i step) (+ acc (luma-sample d i)) (inc c)) (/ acc (double (max 1 c)))))))

(defn- pair-ratio
  "Median of b/a over the pixels where both frames are usable (0.05-0.9), or nil."
  [a b]
  (let [^floats da (:data a) ^floats db (:data b)
        n (* (long (:width a)) (long (:height a))) step (max 1 (quot n 40000))
        rs (vec (sort (for [i (range 0 n step)
                            :let [x (luma-sample da i) y (luma-sample db i)]
                            :when (and (< 0.05 x 0.9) (< 0.05 y 0.9))]
                        (/ y x))))]
    (when (seq rs) (nth rs (quot (count rs) 2)))))

(defn estimate-factors
  "Relative exposure of each image against image `ref`, from the pictures
  themselves: the frames are put in order of brightness and each neighbouring
  pair is related by the median ratio of the pixels both expose well (0.05-0.9);
  the ratios are chained to the reference, so a bracket several stops wide works.
  The images must already be aligned. Returns a vector, 1.0 for the reference."
  [imgs ref]
  (let [order (vec (sort-by #(mean-brightness (nth imgs %)) (range (count imgs))))
        pos (zipmap order (range))
        ;; step.[k] = factor of order[k+1] against order[k]
        steps (vec (for [k (range (dec (count order)))]
                     (or (pair-ratio (nth imgs (order k)) (nth imgs (order (inc k))))
                         (throw (ex-info "The frames do not share enough well-exposed pixels to estimate their exposures"
                                         {:between [(order k) (order (inc k))]})))))
        ;; factor of order[k] against order[0]
        cum (vec (reductions * 1.0 steps))
        ref-f (cum (pos ref))]
    (vec (map-indexed (fn [i _] (/ (double (cum (pos i))) ref-f)) imgs))))

;; ----------------------------------------------------------------- alignment

(defn- gray-mat
  "Float grey Mat of the perceptual (sRGB-encoded) brightness of a scene image: what
  the alignment compares, so a frame that is two stops darker still lines up."
  ^Mat [{:keys [^long width ^long height data]}]
  (let [^floats d data n (* width height) ^floats g (float-array n)
        m (Mat. (int height) (int width) opencv_core/CV_32FC1)]
    (dotimes [i n] (aset g i (float (scene/srgb-encode-extended (luma-sample d i)))))
    (.put (FloatPointer. (.data m)) g)
    m))

(defn shift-image
  "`img` moved by (dx, dy) pixels (positive = right / down); the edges repeat."
  [{:keys [^long width ^long height data]} ^long dx ^long dy]
  (if (and (zero? dx) (zero? dy))
    {:width width :height height :data data}
    (let [^floats src data ^floats out (float-array (alength src))]
      (core/parallel-ranges!
        height
        (fn [^long y0 ^long y1]
          (loop [y y0]
            (when (< y y1)
              (let [sy (Math/max 0 (Math/min (dec height) (- y dy)))]
                (dotimes [x width]
                  (let [sx (Math/max 0 (Math/min (dec width) (- x dx)))
                        o (* 3 (+ (* y width) x)) s (* 3 (+ (* sy width) sx))]
                    (aset out o (aget src s)) (aset out (+ o 1) (aget src (+ s 1))) (aset out (+ o 2) (aget src (+ s 2))))))
              (recur (inc y))))))
      (scene/image width height out))))

(def ^:private align-side 1024)

(defn- measured-shift
  "[dx dy] by which `img` is displaced against `ref-img` (positive = to the right /
  down), by phase correlation of brightness on reduced copies (sub-pixel accurate,
  unaffected by the exposure difference), rounded to whole pixels of the full
  size. [0 0] when the two have nothing in common (low correlation)."
  [ref-img img]
  (let [a (scene/fit ref-img align-side) b (scene/fit img align-side)
        k (/ (double (:width ref-img)) (double (:width a)))
        ma (gray-mat a) mb (gray-mat b) win (Mat.)
        resp (double-array 1)]
    (try
      (opencv_imgproc/createHanningWindow win (Size. (int (:width a)) (int (:height a))) opencv_core/CV_32F)
      (let [^Point2d p (opencv_imgproc/phaseCorrelate ma mb win resp)]
        (if (< (aget resp 0) 0.05)
          [0 0]
          [(long (Math/round (* k (.x p)))) (long (Math/round (* k (.y p))))]))
      (finally (.close ma) (.close mb) (.close win)))))

(defn align
  "The images moved to line up with image `ref` (translation only: the small
  shifts of a hand-held bracket, not rotation). Returns {:images [..] :shifts
  [[dx dy] ..]}: the displacement each frame was found to have (0 for the
  reference); each frame is moved back by that amount."
  [imgs ref]
  (let [shifts (vec (map-indexed (fn [k img] (if (== k ref) [0 0] (measured-shift (nth imgs ref) img))) imgs))]
    {:images (vec (map-indexed (fn [k img] (let [[dx dy] (shifts k)] (shift-image img (- (long dx)) (- (long dy))))) imgs))
     :shifts shifts}))

;; --------------------------------------------------------------------- merge

(defn- smoothstep ^double [^double e0 ^double e1 ^double x]
  (let [t (Math/max 0.0 (Math/min 1.0 (/ (- x e0) (- e1 e0))))] (* t t (- 3.0 (* 2.0 t)))))

(defn- unclipped
  "How far a pixel is from clipping in a frame (1 = safe, 0 = blown): judged by
  its brightest channel, since one clipped channel shifts the colour."
  ^double [^double r ^double g ^double b]
  (- 1.0 (smoothstep 0.85 0.98 (Math/max r (Math/max g b)))))

(defn merge-hdr
  "Merges `imgs` (aligned float scene images of one scene, same size) with their
  relative exposures `factors` (brightness of each against the reference, e.g.
  0.25 for two stops under; see exposure-value and estimate-factors). Each channel
  of each pixel is the weighted mean of the frames' values divided by their
  factor; a frame's weight is its distance from clipping, times a small floor plus
  how far the channel is above the noise, times its factor (a longer exposure is
  quieter). Where every frame is clipped the shortest one is used. The result is
  in the reference's units: a pixel the reference exposed to 0.5 stays 0.5, and
  highlights it clipped come out above 1."
  [imgs factors]
  (let [n (count imgs)
        {:keys [^long width ^long height]} (first imgs)
        np (* width height)
        ^"[[F" srcs (into-array (Class/forName "[F") (map :data imgs))
        ^doubles f (double-array factors)
        shortest (long (first (apply min-key second (map-indexed vector factors))))
        ^floats out (float-array (* 3 np))]
    (core/parallel-ranges!
      np
      (fn [^long start ^long end]
        (let [sum (double-array 3) wsum (double-array 3)]
          (loop [i start]
            (when (< i end)
              (let [j (* 3 i)]
                (java.util.Arrays/fill sum 0.0) (java.util.Arrays/fill wsum 0.0)
                (let [any-ok (loop [k 0 ok 0.0]
                               (if (< k n)
                                 (let [^floats d (aget srcs k)
                                       r (double (aget d j)) g (double (aget d (+ j 1))) b (double (aget d (+ j 2)))
                                       cw (unclipped r g b)
                                       fk (aget f k)]
                                   (dotimes [c 3]
                                     (let [z (double (aget d (+ j c)))
                                           w (* cw fk (+ 0.02 (smoothstep 0.01 0.08 z)))]
                                       (aset sum c (+ (aget sum c) (/ (* w z) fk)))
                                       (aset wsum c (+ (aget wsum c) w))))
                                   (recur (inc k) (+ ok cw)))
                                 ok))]
                  (if (> any-ok 1e-6)
                    (dotimes [c 3] (aset out (+ j c) (float (/ (aget sum c) (aget wsum c)))))
                    (let [^floats d (aget srcs shortest) fk (aget f shortest)]
                      (dotimes [c 3] (aset out (+ j c) (float (/ (double (aget d (+ j c))) fk))))))))
              (recur (inc i)))))))
    (scene/image width height out)))

(defn merge-bracket
  "The whole job for `frames`, a vector of {:scene float image :tags exif tags}:
  align to the middle exposure, take exposure ratios from the EXIF settings (or
  from the pictures when any frame lacks them), merge. Returns
  {:scene merged :factors [..] :shifts [..] :source :exif|:estimated}."
  [frames {:keys [align?] :or {align? true}}]
  (let [n (count frames)
        _ (when (< n 2) (throw (ex-info "Need at least two frames to merge" {})))
        _ (when-not (apply = (map (juxt (comp :width :scene) (comp :height :scene)) frames))
            (throw (ex-info "The frames are not the same size" {})))
        evs (mapv (comp exposure-value :tags) frames)
        exif? (and (every? some? evs) (> (apply max evs) (apply min evs)))
        brightness (mapv (comp mean-brightness :scene) frames)
        order (vec (sort-by (if exif? evs brightness) (range n)))
        ref (nth order (quot n 2))
        {:keys [images shifts]} (if align?
                                  (align (mapv :scene frames) ref)
                                  {:images (mapv :scene frames) :shifts (vec (repeat n [0 0]))})
        factors (if exif?
                  (mapv #(/ (double %) (double (evs ref))) evs)
                  (estimate-factors images ref))]
    {:scene (merge-hdr images factors) :factors factors :shifts shifts :source (if exif? :exif :estimated)}))
