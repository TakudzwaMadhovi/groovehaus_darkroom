(ns darkroom.imaging.geometry
  "Geometry on float scene images: crop, straighten, 90-degree turns, flips,
  perspective (keystone), lens distortion and chromatic-aberration correction
  (by slider or from a lens profile, see darkroom.imaging.lens),
  and resizing. Everything is a single bilinear resampling pass in linear light
  (`geometry`), with the zoom that keeps the frame filled worked out
  numerically. Pure logic, no UI dependency.

  Order of operations, source to output:
    lens correction (distortion, CA) -> turn by :rotate quarter turns ->
    perspective -> straighten by :angle (zoomed to fill) -> flips -> crop.
  The crop is a rectangle of the resulting frame, so adjusting it never changes
  the zoom."
  (:require [darkroom.imaging.core :as core]
            [darkroom.imaging.scene :as scene]))

(set! *unchecked-math* :warn-on-boxed)

(def defaults
  "Neutral values for every geometry setting."
  {:angle 0.0 :aspect "orig" :flip false :flip-v false :rotate 0 :crop nil
   :persp-v 0.0 :persp-h 0.0 :distortion 0.0 :ca-red 0.0 :ca-blue 0.0
   :lens-profile nil})

(def geometry-keys (vec (keys defaults)))

(def aspect-ratios
  "Crop aspect ratios; \"orig\" keeps the frame's own."
  {"1:1" 1.0 "4:5" 0.8 "16:9" (/ 16.0 9.0) "3:2" 1.5})

(defn geometry-neutral?
  [settings]
  (every? (fn [k] (= (get settings k (defaults k)) (defaults k))) geometry-keys))

(defn- setting ^double [settings k] (double (or (get settings k) (defaults k))))

;; --------------------------------------------------------- frame and crop

(defn frame-size
  "[w h] of the frame (the image after the quarter turns, before cropping)."
  [iw ih rotate]
  (if (odd? (long rotate)) [ih iw] [iw ih]))

(defn- clamp01 ^double [^double x] (max 0.0 (min 1.0 x)))

(defn crop-rect
  "[x y w h] of the crop in frame pixels. An explicit :crop (fractions of the
  frame) wins; otherwise a centred rectangle of the :aspect ratio; otherwise the
  whole frame. Always inside the frame and at least one pixel."
  [settings fw fh]
  (let [fw (double fw) fh (double fh)]
    (if-let [[x y w h] (:crop settings)]
      (let [x0 (* fw (clamp01 (double x))) y0 (* fh (clamp01 (double y)))
            x1 (* fw (clamp01 (+ (double x) (double w)))) y1 (* fh (clamp01 (+ (double y) (double h))))
            w' (max 1.0 (- x1 x0)) h' (max 1.0 (- y1 y0))
            x0 (min x0 (- fw w')) y0 (min y0 (- fh h'))]
        [x0 y0 w' h'])
      (let [aspect (get settings :aspect "orig")
            ar     (if (= aspect "orig") (/ fw fh) (double (aspect-ratios aspect (/ fw fh))))
            cw     (min fw (* fh ar))
            ch     (/ cw ar)]
        [(/ (- fw cw) 2.0) (/ (- fh ch) 2.0) cw ch]))))

(defn crop-fractions
  "The crop as fractions [x y w h] of the frame, for drawing an overlay."
  [settings fw fh]
  (let [[x y w h] (crop-rect settings fw fh)
        fw (double fw) fh (double fh)]
    [(/ (double x) fw) (/ (double y) fh) (/ (double w) fw) (/ (double h) fh)]))

(defn output-size
  "[w h] in pixels of `geometry`'s result for a source of iw x ih."
  [settings iw ih]
  (let [[fw fh] (frame-size iw ih (get settings :rotate 0))
        [_ _ w h] (crop-rect settings fw fh)]
    [(max 1 (Math/round (double w))) (max 1 (Math/round (double h)))]))

;; ------------------------------------------------------------- lens profile

(defn- profile-coefficients
  "Radial polynomial k(rho) = 1 + c1 rho + c2 rho^2 + c3 rho^3 + c4 rho^4 as a
  double array [c1 c2 c3 c4] for a profile {:model :poly3 / :poly5 / :ptlens :terms [..]}."
  ^doubles [{:keys [model terms]}]
  (let [t (vec terms) at (fn [i] (double (get t i 0.0)))]
    (case model
      :poly3  (double-array [0.0 (at 0) 0.0 0.0])
      :poly5  (double-array [0.0 (at 0) 0.0 (at 1)])
      :ptlens (double-array [(at 2) (at 1) (at 0) 0.0])
      (throw (ex-info "Unknown lens distortion model" {:model model})))))

(defn- profile-rho-scale
  "Factor turning a source-pixel radius into the profile's radius unit (focal
  lengths on the sensor, as lensfun's NormScale defines it): sensor diagonal /
  crop factor / real focal length / pixel diagonal. `:unit` of the profile holds
  the first three; the pixel diagonal is that of this (possibly preview-sized) image."
  ^double [{:keys [unit]} ^double iw ^double ih]
  (/ (double unit) (Math/hypot iw ih)))

(defn lens-profile-k
  "Radius scale of the profile at profile radius `rho` (output coordinates to
  source coordinates, as in lensfun's distortion modifiers)."
  ^double [^doubles c ^double rho]
  (+ 1.0 (* rho (+ (aget c 0) (* rho (+ (aget c 1) (* rho (+ (aget c 2) (* rho (aget c 3))))))))))

;; ------------------------------------------------------------- the mapping

(def ^:private ^:const persp-strength 0.35)
(def ^:private ^:const distortion-strength 0.25)
(def ^:private ^:const ca-strength 0.005)

;; source-point and fill-zoom run a few thousand times per render (not per
;; pixel), so they are written for clarity: boxed maths is fine here.
(set! *unchecked-math* false)

(defn- source-point
  "Where output frame point (x, y) (pixels from the frame centre) comes from in
  the source (pixels from the source centre), for zoom `z`; `ca` scales the
  radius (chromatic aberration). Mirrors the per-pixel code in `geometry`."
  [{:keys [iw ih fw fh rot cos-t sin-t flip-h? flip-v? pv ph dist lp-coef lp-scale]} z ca x y]
  (let [iw (double iw) ih (double ih) fw (double fw) fh (double fh) rot (long rot)
        cos-t (double cos-t) sin-t (double sin-t) pv (double pv) ph (double ph) dist (double dist)
        z (double z) ca (double ca) x (double x) y (double y)
        nx (/ x (* 0.5 fw)) ny (/ y (* 0.5 fh))
        x1 (* x (+ 1.0 (* persp-strength pv ny)))
        y1 (* y (+ 1.0 (* persp-strength ph nx)))
        x2 (/ x1 z) y2 (/ y1 z)
        rx (+ (* x2 cos-t) (* y2 sin-t))
        ry (- (* y2 cos-t) (* x2 sin-t))
        rx (if flip-h? (- rx) rx)
        ry (if flip-v? (- ry) ry)
        [sx0 sy0] (case rot 0 [rx ry] 1 [ry (- rx)] 2 [(- rx) (- ry)] [(- ry) rx])
        r2 (/ (+ (* sx0 sx0) (* sy0 sy0)) (* 0.25 (+ (* iw iw) (* ih ih))))
        kp (if lp-coef (lens-profile-k lp-coef (* (double lp-scale) (Math/sqrt (+ (* sx0 sx0) (* sy0 sy0))))) 1.0)
        k  (* (- 1.0 (* distortion-strength dist r2)) kp (+ 1.0 ca))]
    [(* sx0 k) (* sy0 k)]))

(defn- fill-zoom
  "Smallest zoom >= 1 at which every output pixel centre on the frame's border,
  for any chromatic-aberration scale, comes from inside the source (between its
  outermost pixel centres, so no sample is ever clamped)."
  [params]
  (let [{:keys [fw fh iw ih ca-extremes]} params
        hw (- (* 0.5 (double fw)) 0.5) hh (- (* 0.5 (double fh)) 0.5)
        n  24
        lerp (fn [a b i] (+ a (* (- b a) (/ (double i) n))))
        border (concat (for [i (range (inc n))] [(lerp (- hw) hw i) (- hh)])
                       (for [i (range (inc n))] [(lerp (- hw) hw i) hh])
                       (for [i (range (inc n))] [(- hw) (lerp (- hh) hh i)])
                       (for [i (range (inc n))] [hw (lerp (- hh) hh i)]))
        eps 1e-9
        inside? (fn [z]
                  (every? (fn [[x y]]
                            (every? (fn [ca]
                                      (let [[sx sy] (source-point params z ca x y)]
                                        (and (<= (Math/abs (double sx)) (+ eps (- (* 0.5 (double iw)) 0.5)))
                                             (<= (Math/abs (double sy)) (+ eps (- (* 0.5 (double ih)) 0.5))))))
                                    ca-extremes))
                          border))]
    (if (inside? 1.0)
      1.0
      (loop [lo 1.0 hi 8.0 i 0]
        (if (or (>= i 40) (< (- hi lo) 1e-7))
          hi
          (let [mid (* 0.5 (+ lo hi))]
            (if (inside? mid) (recur lo mid (inc i)) (recur mid hi (inc i)))))))))

(defn- mapping-params
  "The numbers `source-point`, `fill-zoom` and `geometry` share, for a source of
  iw x ih."
  [settings iw ih]
  (let [rot (long (mod (long (get settings :rotate 0)) 4))
        [fw fh] (frame-size iw ih rot)
        th (Math/toRadians (setting settings :angle))
        lp   (:lens-profile settings)
        [lp-kr lp-kb] (:ca lp)
        ca-r (+ (* ca-strength (setting settings :ca-red)) (if lp-kr (- (double lp-kr) 1.0) 0.0))
        ca-b (+ (* ca-strength (setting settings :ca-blue)) (if lp-kb (- (double lp-kb) 1.0) 0.0))]
    {:lp-coef (when lp (profile-coefficients lp))
     :lp-scale (when lp (profile-rho-scale lp (double iw) (double ih)))
     :iw (double iw) :ih (double ih) :fw (double fw) :fh (double fh) :rot rot
     :cos-t (Math/cos th) :sin-t (Math/sin th)
     :flip-h? (boolean (:flip settings)) :flip-v? (boolean (:flip-v settings))
     :pv (setting settings :persp-v) :ph (setting settings :persp-h) :dist (setting settings :distortion)
     :ca-r ca-r :ca-b ca-b :ca-extremes (distinct [0.0 ca-r ca-b])}))

(defn zoom-for
  "The zoom (>= 1) that keeps the frame filled for these settings on a source
  of iw x ih."
  [settings iw ih]
  (fill-zoom (mapping-params settings iw ih)))

(set! *unchecked-math* :warn-on-boxed)

(defmacro ^:private sample-channel!
  "Writes channel `c` of the bilinear sample of float RGB `src` (row width `w`,
  height `h`) at (sx, sy) into `out` at float offset `o` + c, clamping at the
  edges."
  [src w h c sx sy out o]
  `(let [sx#  ~sx sy# ~sy
         x0#  (Math/floor sx#) y0# (Math/floor sy#)
         fx#  (- sx# x0#)      fy# (- sy# y0#)
         xi#  (long x0#)       yi# (long y0#)
         xa#  (max 0 (min (dec ~w) xi#))   xb# (max 0 (min (dec ~w) (inc xi#)))
         ya#  (max 0 (min (dec ~h) yi#))   yb# (max 0 (min (dec ~h) (inc yi#)))
         a#   (double (aget ~src (+ (* 3 (+ (* ya# ~w) xa#)) ~c)))
         b#   (double (aget ~src (+ (* 3 (+ (* ya# ~w) xb#)) ~c)))
         c0#  (double (aget ~src (+ (* 3 (+ (* yb# ~w) xa#)) ~c)))
         d#   (double (aget ~src (+ (* 3 (+ (* yb# ~w) xb#)) ~c)))]
     (aset ~out (+ ~o ~c)
           (float (+ (* (- 1.0 fy#) (+ (* (- 1.0 fx#) a#) (* fx# b#)))
                     (* fy# (+ (* (- 1.0 fx#) c0#) (* fx# d#))))))))

(defn geometry
  "Applies the geometry settings (see the namespace doc and `defaults`) to a
  scene image: one bilinear resampling pass in linear light. Unchanged settings
  return the same image."
  [{:keys [^long width ^long height data] :as img} settings]
  (if (geometry-neutral? settings)
    img
    (let [{:keys [rot pv ph dist ca-r ca-b cos-t sin-t] :as params} (mapping-params settings width height)
          rot    (long rot)
          [fw fh] (frame-size width height rot)
          [cx cy cw ch] (crop-rect settings fw fh)
          w      (max 1 (Math/round (double cw)))
          h      (max 1 (Math/round (double ch)))
          pv     (double pv) ph (double ph) dist (double dist)
          ca-r   (double ca-r) ca-b (double ca-b)
          cos-t  (double cos-t) sin-t (double sin-t)
          z      (double (fill-zoom params))
          flip-h? (boolean (:flip settings))
          flip-v? (boolean (:flip-v settings))
          persp? (or (not (zero? pv)) (not (zero? ph)))
          lens?  (not (zero? dist))
          ^doubles lp-coef (:lp-coef params)
          lp?    (some? lp-coef)
          lp-scale (double (or (:lp-scale params) 0.0))
          ca?    (or (not (zero? ca-r)) (not (zero? ca-b)))
          iw     (double width) ih (double height)
          cx0    (double cx) cy0 (double cy)
          half-w (* 0.5 (double fw)) half-h (* 0.5 (double fh))
          inv-r2 (/ 1.0 (* 0.25 (+ (* iw iw) (* ih ih))))
          ^floats src data
          ^floats out (float-array (* 3 w h))]
      (core/parallel-ranges!
        (* w h)
        (fn [^long start ^long end]
          (loop [i start]
            (when (< i end)
              (let [x  (- (+ cx0 (rem i w) 0.5) half-w)
                    y  (- (+ cy0 (quot i w) 0.5) half-h)
                    ;; perspective
                    x1 (if persp? (* x (+ 1.0 (* persp-strength pv (/ y half-h)))) x)
                    y1 (if persp? (* y (+ 1.0 (* persp-strength ph (/ x half-w)))) y)
                    ;; zoom, straighten, flips
                    x2 (/ x1 z) y2 (/ y1 z)
                    rx (+ (* x2 cos-t) (* y2 sin-t))
                    ry (- (* y2 cos-t) (* x2 sin-t))
                    rx (if flip-h? (- rx) rx)
                    ry (if flip-v? (- ry) ry)
                    ;; quarter turns back into source orientation
                    sx0 (case rot 0 rx 1 ry 2 (- rx) (- ry))
                    sy0 (case rot 0 ry 1 (- rx) 2 (- ry) rx)
                    ;; lens distortion
                    k   (* (if lens? (- 1.0 (* distortion-strength dist (* (+ (* sx0 sx0) (* sy0 sy0)) inv-r2))) 1.0)
                           (if lp? (lens-profile-k lp-coef (* lp-scale (Math/sqrt (+ (* sx0 sx0) (* sy0 sy0))))) 1.0))
                    sx  (* sx0 k) sy (* sy0 k)
                    o   (* 3 i)]
                (if ca?
                  (let [kr (+ 1.0 ca-r) kb (+ 1.0 ca-b)]
                    (sample-channel! src width height 0 (- (+ (* sx kr) (* 0.5 iw)) 0.5) (- (+ (* sy kr) (* 0.5 ih)) 0.5) out o)
                    (sample-channel! src width height 1 (- (+ sx (* 0.5 iw)) 0.5) (- (+ sy (* 0.5 ih)) 0.5) out o)
                    (sample-channel! src width height 2 (- (+ (* sx kb) (* 0.5 iw)) 0.5) (- (+ (* sy kb) (* 0.5 ih)) 0.5) out o))
                  (let [px (- (+ sx (* 0.5 iw)) 0.5) py (- (+ sy (* 0.5 ih)) 0.5)]
                    (sample-channel! src width height 0 px py out o)
                    (sample-channel! src width height 1 px py out o)
                    (sample-channel! src width height 2 px py out o))))
              (recur (inc i))))))
      (scene/image w h out))))

;; ----------------------------------------------------------------- resize

(defn resize-long-edge
  "Scales `img` so its longest side is `edge` px (never enlarges). Large
  reductions box-average first (scene/fit), then bilinear to the exact size."
  [{:keys [^long width ^long height] :as img} edge]
  (let [long-side (max width height)]
    (if (or (nil? edge) (<= (long edge) 0) (<= long-side (long edge)))
      img
      (let [pre (scene/fit img (* 2 (long edge))) ; at most 2x too big: cheap box pass
            pw  (long (:width pre))
            ph  (long (:height pre))
            s   (/ (double (long edge)) (max pw ph))
            w   (max 1 (Math/round (* pw s)))
            h   (max 1 (Math/round (* ph s)))
            ^floats src (:data pre)
            ^floats out (float-array (* 3 w h))]
        (core/parallel-ranges!
          (* w h)
          (fn [^long start ^long end]
            (loop [i start]
              (when (< i end)
                (let [sx (- (* (+ (rem i w) 0.5) (/ (double pw) w)) 0.5)
                      sy (- (* (+ (quot i w) 0.5) (/ (double ph) h)) 0.5)
                      o  (* 3 i)]
                  (sample-channel! src pw ph 0 sx sy out o)
                  (sample-channel! src pw ph 1 sx sy out o)
                  (sample-channel! src pw ph 2 sx sy out o))
                (recur (inc i))))))
        (scene/image w h out)))))
