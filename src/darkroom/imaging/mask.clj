(ns darkroom.imaging.mask
  "Masks for local adjustments: a mask is a float plane (w*h values in 0-1, 1 =
  fully affected). Geometric masks (linear and radial gradients, brush strokes)
  come from coordinates given as fractions of the image; range masks
  (luminance, colour) are read from the picture; subject and sky selections are
  classical image-analysis approximations (GrabCut from a rectangle; colour,
  brightness and smoothness connected to the top edge), unless a salient-object
  network (U2-Net style, ONNX) is supplied: `subject-ai`. Pure logic (OpenCV for
  GrabCut and the network), no UI dependency."
  (:require [darkroom.imaging.color :as color]
            [darkroom.imaging.core :as core]
            [darkroom.imaging.detail :as detail]
            [darkroom.imaging.scene :as scene])
  (:import (org.bytedeco.javacpp BytePointer FloatPointer IntPointer)
           (org.bytedeco.opencv.global opencv_core opencv_dnn opencv_imgproc)
           (org.bytedeco.opencv.opencv_core Mat MatVector Rect Size StringVector)
           (org.bytedeco.opencv.opencv_dnn Net)))

(set! *unchecked-math* :warn-on-boxed)

(defn- smoothstep ^double [^double x]
  (let [t (max 0.0 (min 1.0 x))] (* t t (- 3.0 (* 2.0 t)))))

(defn- plane ^floats [^long w ^long h] (float-array (* w h)))

(defn ones ^floats [^long w ^long h]
  (let [^floats p (plane w h)] (java.util.Arrays/fill p (float 1.0)) p))

;; ---------------------------------------------------------------- gradients

(defn linear
  "Linear gradient mask: 1 at and behind the start point (x0, y0), fading
  smoothly to 0 at the end point (x1, y1); both are fractions of the image."
  ^floats [^long w ^long h {:keys [x0 y0 x1 y1]}]
  (let [ax (* (double x0) w) ay (* (double y0) h)
        vx (- (* (double x1) w) ax) vy (- (* (double y1) h) ay)
        vv (+ (* vx vx) (* vy vy))
        ^floats out (plane w h)]
    (if (< vv 1e-9)
      (ones w h)
      (do (core/parallel-ranges!
            (* w h)
            (fn [^long start ^long end]
              (loop [i start]
                (when (< i end)
                  (let [px (+ (rem i w) 0.5) py (+ (quot i w) 0.5)
                        t  (/ (+ (* (- px ax) vx) (* (- py ay) vy)) vv)]
                    (aset out i (float (- 1.0 (smoothstep t)))))
                  (recur (inc i))))))
          out))))

(defn radial
  "Elliptical gradient mask centred on (cx, cy) with radii rx, ry (fractions of
  the width and height): 1 inside, 0 outside, with a soft edge `feather` (0-1) of
  the radius wide."
  ^floats [^long w ^long h {:keys [cx cy rx ry feather]}]
  (let [cxp (* (double cx) w) cyp (* (double cy) h)
        rxp (max 1e-6 (* (double rx) w)) ryp (max 1e-6 (* (double ry) h))
        f   (max 0.001 (min 1.0 (double (or feather 0.5))))
        inner (- 1.0 f)
        ^floats out (plane w h)]
    (core/parallel-ranges!
      (* w h)
      (fn [^long start ^long end]
        (loop [i start]
          (when (< i end)
            (let [dx (/ (- (+ (rem i w) 0.5) cxp) rxp) dy (/ (- (+ (quot i w) 0.5) cyp) ryp)
                  d  (Math/sqrt (+ (* dx dx) (* dy dy)))]
              (aset out i (float (- 1.0 (smoothstep (/ (- d inner) f))))))
            (recur (inc i))))))
    out))

;; -------------------------------------------------------------------- brush

(defn- stamp!
  "Paints (or, when erase? is true, removes) a soft round dab on mask `m`."
  [^floats m w h cx cy r feather flow erase?]
  (let [w (long w) h (long h) cx (double cx) cy (double cy) r (double r)
        feather (double feather) flow (double flow)
        hard (- 1.0 feather)
        x0 (max 0 (long (Math/floor (- cx r)))) x1 (min (dec w) (long (Math/ceil (+ cx r))))
        y0 (max 0 (long (Math/floor (- cy r)))) y1 (min (dec h) (long (Math/ceil (+ cy r))))]
    (loop [y y0]
      (when (<= y y1)
        (loop [x x0]
          (when (<= x x1)
            (let [dx (- (+ x 0.5) cx) dy (- (+ y 0.5) cy)
                  d  (/ (Math/sqrt (+ (* dx dx) (* dy dy))) r)]
              (when (< d 1.0)
                (let [profile (if (<= d hard) 1.0 (- 1.0 (smoothstep (/ (- d hard) (max 1e-6 feather)))))
                      a (* flow profile)
                      i (+ (* y w) x)
                      v (double (aget m i))]
                  (aset m i (float (if erase? (* v (- 1.0 a)) (+ v (* (- 1.0 v) a))))))))
            (recur (inc x))))
        (recur (inc y))))))

(defn brush
  "Mask painted by `strokes`: each {:points [[x y] ...] (fractions of the image)
  :radius (fraction of the long edge) :feather 0-1 :flow 0-1 :erase? bool}, applied in
  order (an erase stroke removes what was painted before it)."
  ^floats [^long w ^long h strokes]
  (let [^floats m (plane w h) long-e (double (max w h))]
    (doseq [{:keys [points radius feather flow erase?] :or {feather 0.5 flow 1.0}} strokes]
      (let [r (max 1.0 (* (double radius) long-e))
            spacing (max 1.0 (* 0.2 r))
            pts (mapv (fn [[x y]] [(* (double x) w) (* (double y) h)]) points)
            dab! (fn [[x y]] (stamp! m w h (double x) (double y) r (double feather) (double flow) (boolean erase?)))]
        (when (seq pts)
          (dab! (first pts))
          (doseq [[[ax ay] [bx by]] (partition 2 1 pts)]
            (let [dx (- (double bx) (double ax)) dy (- (double by) (double ay))
                  len (Math/sqrt (+ (* dx dx) (* dy dy)))
                  n (max 1 (long (Math/ceil (/ len spacing))))]
              (dotimes [k n]
                (let [t (/ (double (inc k)) n)] (dab! [(+ (double ax) (* t dx)) (+ (double ay) (* t dy))]))))))))
    m))

;; ------------------------------------------------------------- range masks

(defn luminance
  "Mask from brightness: 1 for encoded luma between `lo` and `hi` (0-1), with
  soft edges `smooth` (0-1) wide on each side."
  ^floats [{:keys [^long width ^long height data]} {:keys [lo hi smooth]}]
  (let [lo (double (or lo 0.0)) hi (double (or hi 1.0)) s (max 0.001 (double (or smooth 0.1)))
        ^floats src data
        ^doubles lw (double-array (color/luma-weights :working))
        lr (aget lw 0) lg (aget lw 1) lb (aget lw 2)
        ^floats out (plane width height)]
    (core/parallel-ranges!
      (* width height)
      (fn [^long start ^long end]
        (loop [i start]
          (when (< i end)
            (let [j (* 3 i)
                  y (scene/srgb-encode-extended (+ (* lr (aget src j)) (* lg (aget src (+ j 1))) (* lb (aget src (+ j 2)))))
                  a (if (<= lo 0.0) 1.0 (smoothstep (/ (- y (- lo s)) (* 2.0 s))))
                  b (if (>= hi 1.0) 1.0 (- 1.0 (smoothstep (/ (- y (- hi s)) (* 2.0 s)))))]
              (aset out i (float (* a b))))
            (recur (inc i))))))
    out))

(defn- hue-of
  "Hue in degrees (0-360) and chroma of encoded rgb; chroma 0 for greys."
  ^doubles [^double r ^double g ^double b]
  (let [mx (max r (max g b)) mn (min r (min g b)) d (- mx mn)]
    (double-array
      [(if (< d 1e-9) 0.0
           (let [h (* 60.0 (double (cond (== mx r) (let [q (/ (- g b) d)] (- q (* 6.0 (Math/floor (/ q 6.0)))))
                                         (== mx g) (+ 2.0 (/ (- b r) d))
                                         :else     (+ 4.0 (/ (- r g) d)))))]
             (if (neg? h) (+ h 360.0) h)))
       d])))

(defn hue-range
  "Mask from colour: 1 for hues within `range` degrees of `hue` (soft over a
  further half range), faded out for colours with chroma below `min-chroma`
  (so greys are never picked)."
  ^floats [{:keys [^long width ^long height data]} {:keys [hue range min-chroma]}]
  (let [hue (double hue) rng (max 1.0 (double (or range 20.0))) cmin (max 0.005 (double (or min-chroma 0.05)))
        ^floats src data
        ^floats out (plane width height)]
    (core/parallel-ranges!
      (* width height)
      (fn [^long start ^long end]
        (loop [i start]
          (when (< i end)
            (let [j (* 3 i)
                  ^doubles hc (hue-of (scene/srgb-encode-extended (aget src j))
                             (scene/srgb-encode-extended (aget src (+ j 1)))
                             (scene/srgb-encode-extended (aget src (+ j 2))))
                  dh (let [d (Math/abs (- (aget hc 0) hue))] (min d (- 360.0 d)))
                  m  (- 1.0 (smoothstep (/ (- dh rng) (* 0.5 rng))))
                  g  (smoothstep (/ (aget hc 1) (* 2.0 cmin)))]
              (aset out i (float (* m g))))
            (recur (inc i))))))
    out))

;; ----------------------------------------------- subject and sky selection

(def ^:private analysis-side 400)

(defn- small-copy [img] (scene/fit img analysis-side))

(defn- resize-plane
  "Bilinear resize of a float plane."
  ^floats [^floats p w h nw nh]
  (if (and (== (long w) (long nw)) (== (long h) (long nh)))
    p
    (let [^Mat m (Mat. (int h) (int w) opencv_core/CV_32FC1) ^Mat o (Mat.)]
      (try (.put (org.bytedeco.javacpp.FloatPointer. (.data m)) p)
           (opencv_imgproc/resize m o (org.bytedeco.opencv.opencv_core.Size. (int nw) (int nh)) 0.0 0.0 opencv_imgproc/INTER_LINEAR)
           (let [out (float-array (* (long nw) (long nh)))] (.get (org.bytedeco.javacpp.FloatPointer. (.data o)) out) out)
           (finally (.close m) (.close o))))))

(defn- feathered
  "A full-size mask from a small plane: resized, then softened by a blur whose
  sigma is `sigma-frac` of the long edge."
  ^floats [^floats small sw sh w h sigma-frac]
  (let [full (resize-plane small sw sh w h)]
    (detail/blur-plane full w h (* (double sigma-frac) (double (max (long w) (long h)))))))

(defn subject
  "Selects the foreground inside `rect` [x y w h] (fractions of the image) with
  GrabCut on a reduced copy: pixels inside the rectangle that differ from the
  colours outside it. A classical approximation of 'select subject'."
  ^floats [{:keys [^long width ^long height] :as img} {:keys [rect]}]
  (let [small (small-copy img)
        sw (long (:width small)) sh (long (:height small))
        [rx ry rw rh] (mapv double rect)
        rx (double rx) ry (double ry) rw (double rw) rh (double rh)
        x (max 1 (min (- sw 3) (long (* rx sw)))) y (max 1 (min (- sh 3) (long (* ry sh))))
        w (max 2 (min (- sw x 1) (long (* rw sw)))) h (max 2 (min (- sh y 1) (long (* rh sh))))
        ^floats d (:data small)
        n (* sw sh)
        ^bytes bgr (byte-array (* 3 n))]
    (dotimes [i n]
      (dotimes [c 3]
        (aset bgr (+ (* 3 i) (- 2 c))
              (unchecked-byte (Math/round (* 255.0 (max 0.0 (min 1.0 (scene/srgb-encode-extended (aget d (+ (* 3 i) c)))))))))))
    (let [img8 (Mat. (int sh) (int sw) opencv_core/CV_8UC3)
          m    (Mat. (int sh) (int sw) opencv_core/CV_8UC1)
          bgd  (Mat.) fgd (Mat.)
          rect (Rect. (int x) (int y) (int w) (int h))]
      (try
        (.put (.data img8) bgr)
        (opencv_imgproc/grabCut img8 m rect bgd fgd 4 opencv_imgproc/GC_INIT_WITH_RECT)
        (let [^bytes mb (byte-array n) ^floats p (plane sw sh)]
          (.get (.data m) mb)
          (dotimes [i n]
            (let [v (long (aget mb i))]
              (aset p i (float (if (or (== v opencv_imgproc/GC_FGD) (== v opencv_imgproc/GC_PR_FGD)) 1.0 0.0)))))
          (feathered p sw sh width height 0.004))
        (finally (.close img8) (.close m) (.close bgd) (.close fgd))))))

;; ------------------------------------------------ subject by neural network

(def ^:private ^:const net-side 320)

(defonce ^:private nets (atom {}))

(defn- net-for
  "The loaded network for the ONNX file at `path` (kept until the file changes).
  Throws ex-info when the file is missing or OpenCV cannot read it as a network."
  ^Net [path]
  (let [f (java.io.File. (str path))
        k [(.getPath f) (.lastModified f)]]
    (or (get @nets k)
        (do (when-not (.isFile f) (throw (ex-info "Segmentation model not found" {:path (str path)})))
            (let [^Net n (try (opencv_dnn/readNetFromONNX (.getPath f))
                              (catch Throwable t (throw (ex-info (str "Not a usable ONNX model: " (.getMessage t)) {:path (str path)}))))]
              (when (.empty n) (throw (ex-info "Not a usable ONNX model" {:path (str path)})))
              (reset! nets {k n})
              n)))))

(defn check-model!
  "Loads the model at `path` (so a bad file is reported when it is chosen, not
  when it is first used). Returns the path."
  [path]
  (net-for path)
  path)

(def ^:private imagenet-mean [0.485 0.456 0.406])
(def ^:private imagenet-std [0.229 0.224 0.225])

(defn- network-input
  "Float NCHW [1 3 320 320] input for a U2-Net style network from `small` (scene
  image): converted to sRGB, squeezed to 320 x 320 (as the network was trained),
  scaled by its maximum and normalised with the ImageNet mean and deviation."
  ^floats [small]
  (let [sw (long (:width small)) sh (long (:height small))
        ^doubles m (double-array (color/convert-matrix :working :srgb))
        ^floats d (:data small)
        n (* sw sh)
        ^floats rgb (float-array (* 3 n))]
    (dotimes [i n]
      (let [j (* 3 i) r (double (aget d j)) g (double (aget d (+ j 1))) b (double (aget d (+ j 2)))
            enc (fn ^double [^double v] (max 0.0 (min 1.0 (scene/srgb-encode-extended v))))]
        (aset rgb j (float (enc (+ (* (aget m 0) r) (* (aget m 1) g) (* (aget m 2) b)))))
        (aset rgb (+ j 1) (float (enc (+ (* (aget m 3) r) (* (aget m 4) g) (* (aget m 5) b)))))
        (aset rgb (+ j 2) (float (enc (+ (* (aget m 6) r) (* (aget m 7) g) (* (aget m 8) b)))))))
    (let [^Mat src (Mat. (int sh) (int sw) opencv_core/CV_32FC3) ^Mat dst (Mat.)]
      (try
        (.put (FloatPointer. (.data src)) rgb)
        (opencv_imgproc/resize src dst (Size. (int net-side) (int net-side)) 0.0 0.0 opencv_imgproc/INTER_AREA)
        (let [np (* net-side net-side) ^floats px (float-array (* 3 np)) ^floats out (float-array (* 3 np))]
          (.get (FloatPointer. (.data dst)) px)
          (let [mx (Math/max 1e-6 (double (loop [i 0 a 0.0] (if (< i (* 3 np)) (recur (inc i) (Math/max a (double (aget px i)))) a))))]
            (dotimes [i np]
              (dotimes [c 3]
                (aset out (+ (* c np) i)
                      (float (/ (- (/ (double (aget px (+ (* 3 i) c))) mx) (double (imagenet-mean c))) (double (imagenet-std c))))))))
          out)
        (finally (.close src) (.close dst))))))

(defn subject-ai
  "Selects the subject with a salient-object network: the ONNX model at
  (:model shape), a U2-Net / U2-Net-small style network (input 320 x 320 RGB,
  first output the fused saliency map). Runs on a reduced copy; the soft result
  is scaled up to the image and feathered. Serialised: the network object is shared."
  ^floats [{:keys [^long width ^long height] :as img} {:keys [model]}]
  (let [^Net net (net-for model)
        small (small-copy img)
        ^floats in (network-input small)
        np (* net-side net-side)]
    (locking net
      (let [^Mat blob (Mat. (IntPointer. (int-array [1 3 net-side net-side])) opencv_core/CV_32F (FloatPointer. in))
            names (.getUnconnectedOutLayersNames net)
            outs (MatVector.)]
        (try
          (.setInput net blob)
          (.forward net outs names)
          (let [^Mat o (.get outs 0) ^floats p (float-array np)]
            (when (not= np (.total o)) (throw (ex-info "Unexpected network output size" {:total (.total o)})))
            (.get (.asFloatBuffer (.asByteBuffer (.capacity (.data o) (* 4 np)))) p)
            (let [lo (double (loop [i 0 a 1.0] (if (< i np) (recur (inc i) (Math/min a (double (aget p i)))) a)))
                  hi (double (loop [i 0 a 0.0] (if (< i np) (recur (inc i) (Math/max a (double (aget p i)))) a)))
                  span (Math/max 1e-6 (- hi lo))]
              (dotimes [i np] (aset p i (float (/ (- (double (aget p i)) lo) span))))
              (feathered p net-side net-side width height 0.003)))
          (finally (.close blob)))))))

(defn- enc-at
  "Encoded value of channel c of pixel i in float RGB `d`."
  ^double [^floats d ^long i ^long c]
  (scene/srgb-encode-extended (aget d (+ (* 3 i) c))))

(defn sky
  "Selects the sky: bright, bluish (or bright and colourless, for overcast) and
  smooth pixels connected to the top edge of the picture. A heuristic, so it
  suits open skies better than busy cloudscapes."
  ^floats [{:keys [^long width ^long height] :as img} _]
  (let [small (small-copy img)
        sw (long (:width small)) sh (long (:height small))
        ^floats d (:data small)
        n (* sw sh)
        ^doubles lw (double-array (color/luma-weights :working))
        luma (double-array n)
        cand (boolean-array n)]
    (dotimes [i n]
      (aset luma i (+ (* (aget lw 0) (enc-at d i 0)) (* (aget lw 1) (enc-at d i 1)) (* (aget lw 2) (enc-at d i 2)))))
    (dotimes [i n]
      (let [x (rem i sw) y (quot i sw)
            r (enc-at d i 0) g (enc-at d i 1) b (enc-at d i 2)
            l (aget luma i)
            gx (if (< x (dec sw)) (Math/abs (- (aget luma (inc i)) l)) 0.0)
            gy (if (< y (dec sh)) (Math/abs (- (aget luma (+ i sw)) l)) 0.0)
            chroma (- (max r g b) (min r g b))
            bluish (and (>= b (- r 0.02)) (>= b (- g 0.05)) (> chroma 0.03))
            overcast (and (< chroma 0.10) (> l 0.55))]
        (aset cand i (boolean (and (> l 0.30) (or bluish overcast) (< (+ gx gy) 0.06))))))
    ;; keep what is connected to the top row
    (let [keep (boolean-array n) stack (java.util.ArrayDeque.)]
      (dotimes [x sw] (when (aget cand x) (aset keep x true) (.push stack (int x))))
      (while (not (.isEmpty stack))
        (let [i (long (.pop stack)) x (rem i sw) y (quot i sw)]
          (dotimes [k 4]
            (let [nx (+ x (case k 0 -1 1 1 0)) ny (+ y (case k 2 -1 3 1 0))]
              (when (and (>= nx 0) (< nx sw) (>= ny 0) (< ny sh))
                (let [j (+ (* ny sw) nx)]
                  (when (and (aget cand j) (not (aget keep j)))
                    (aset keep j true) (.push stack (int j)))))))))
      (let [^floats p (plane sw sh)]
        (dotimes [i n] (aset p i (float (if (aget keep i) 1.0 0.0))))
        (feathered p sw sh width height 0.006)))))
