(ns darkroom.imaging.develop
  "The Develop-view image engine on float scene images (darkroom.imaging.scene):
  geometry (straighten / aspect crop / flip), the tone pipeline, the tone curve
  and resize. Pure logic, no UI dependency.

  Exposure and white balance act in linear light, where they are physically
  meaningful. The remaining tone steps (shadows/highlights, contrast, fade,
  curve, saturation, black & white, vignette, grain) run on the sRGB-encoded
  (perceptual) values of the same pixel, as in the original design engine; those
  values may exceed 1 so highlights can still be recovered, and are clamped only
  at the end.

  Settings are a map; see `defaults`."
  (:require [darkroom.imaging.color :as color]
            [darkroom.imaging.core :as core]
            [darkroom.imaging.scene :as scene]))

(set! *unchecked-math* :warn-on-boxed)

(def default-curve [0.0 0.25 0.5 0.75 1.0])

(def defaults
  "Neutral values for every Develop setting."
  {:exposure 0.0 :contrast 0.0 :highlights 0.0 :shadows 0.0 :temp 0.0 :tint 0.0
   :saturation 0.0 :fade 0.0 :bw 0.0 :grain 0.0 :vignette 0.0
   :angle 0.0 :aspect "orig" :flip false :curve default-curve})

(def tone-keys
  [:exposure :contrast :highlights :shadows :temp :tint :saturation :fade :bw :grain :vignette :curve])

(def geometry-keys [:angle :aspect :flip])

(def aspect-ratios
  "Crop aspect ratios; \"orig\" keeps the image's own."
  {"1:1" 1.0 "4:5" 0.8 "16:9" (/ 16.0 9.0) "3:2" 1.5})

;; ------------------------------------------------------------------ curve

(defn curve-lut
  "256-entry tone curve (floats 0-1) through the 5 control points `cv` at
  x = 0, 1/4, 1/2, 3/4, 1: a cubic Hermite spline using finite-difference
  tangents (as in the design reference)."
  ^doubles [cv]
  (let [^doubles c (double-array cv)
        m   (double-array 5)
        out (double-array 256)]
    (dotimes [i 5]
      (let [a (max 0 (dec i)) b (min 4 (inc i))]
        (aset m i (/ (- (aget c b) (aget c a)) (* 0.25 (- b a))))))
    (dotimes [i 256]
      (let [x  (/ i 255.0)
            k  (long (min 3.0 (Math/floor (/ x 0.25))))
            t  (/ (- x (* 0.25 k)) 0.25)
            t2 (* t t) t3 (* t2 t)
            y  (+ (* (+ (- (* 2 t3) (* 3 t2)) 1) (aget c k))
                  (* (+ (- t3 (* 2 t2)) t) 0.25 (aget m k))
                  (* (+ (* -2 t3) (* 3 t2)) (aget c (inc k)))
                  (* (- t3 t2) 0.25 (aget m (inc k))))]
        (aset out i (max 0.0 (min 1.0 y)))))
    out))


;; ------------------------------------------------------------------- tone

(defn tone-neutral?
  "True when no tone setting differs from the defaults."
  [settings]
  (every? (fn [k] (= (get settings k (defaults k)) (defaults k))) tone-keys))

(defn- setting
  "Numeric setting `k` (default from `defaults`) as a primitive double."
  ^double [settings k]
  (double (get settings k (defaults k))))

(def ^:private working-luma (color/luma-weights :working))

(defn tone
  "Applies the tone pipeline to a scene image and returns a new one. Per pixel:
  white balance and exposure (linear light); then, on the encoded values,
  shadows/highlights, contrast around 0.5, fade, tone curve, saturation,
  black & white mix, vignette, grain; clamp to [0, 1] and back to linear."
  [{:keys [^long width ^long height data]} settings]
  (let [ex      (Math/pow 2.0 (setting settings :exposure))
        ^doubles wm (double-array (map #(* ex (double %))
                                       (color/wb-matrix (setting settings :temp) (setting settings :tint))))
        w0 (aget wm 0) w1 (aget wm 1) w2 (aget wm 2) w3 (aget wm 3) w4 (aget wm 4)
        w5 (aget wm 5) w6 (aget wm 6) w7 (aget wm 7) w8 (aget wm 8)
        ^doubles lw (double-array working-luma)
        lr (aget lw 0) lg (aget lw 1) lb (aget lw 2)
        con     (+ 1.0 (setting settings :contrast))
        sat     (+ 1.0 (setting settings :saturation))
        shd     (setting settings :shadows)
        hil     (setting settings :highlights)
        fade    (setting settings :fade)
        bw      (setting settings :bw)
        vig     (setting settings :vignette)
        grain   (setting settings :grain)
        ^doubles lut (curve-lut (get settings :curve default-curve))
        ^floats src data
        ^floats out (float-array (alength src))
        k       (- 1.0 (* fade 0.25))
        f2      (* fade 0.12)]
    (core/parallel-ranges!
      (quot (alength src) 3)
      (fn [^long start ^long end]
        (loop [i start]
          (when (< i end)
            (let [j  (* 3 i)
                  x  (rem i width)
                  y  (quot i width)
                  sr (double (aget src j)) sg (double (aget src (+ j 1))) sb (double (aget src (+ j 2)))
                  ;; linear light: white balance + exposure
                  r0 (+ (* w0 sr) (* w1 sg) (* w2 sb))
                  g0 (+ (* w3 sr) (* w4 sg) (* w5 sb))
                  b0 (+ (* w6 sr) (* w7 sg) (* w8 sb))
                  ;; perceptual domain
                  er (scene/srgb-encode-extended r0)
                  eg (scene/srgb-encode-extended g0)
                  eb (scene/srgb-encode-extended b0)
                  l  (max 0.0 (min 1.0 (scene/srgb-encode-extended (+ (* lr r0) (* lg g0) (* lb b0)))))
                  sh (+ (* shd 0.35 (- 1.0 l) (- 1.0 l)) (* hil 0.35 l l))
                  r1 (+ (* (- (+ er sh) 0.5) con) 0.5)
                  g1 (+ (* (- (+ eg sh) 0.5) con) 0.5)
                  b1 (+ (* (- (+ eb sh) 0.5) con) 0.5)
                  r2 (+ (* r1 k) f2) g2 (+ (* g1 k) f2) b2 (+ (* b1 k) f2)
                  r3 (scene/lut-at lut r2)
                  g3 (scene/lut-at lut g2)
                  b3 (scene/lut-at lut b2)
                  l2 (+ (* lr r3) (* lg g3) (* lb b3))
                  r4 (+ l2 (* (- r3 l2) sat))
                  g4 (+ l2 (* (- g3 l2) sat))
                  b4 (+ l2 (* (- b3 l2) sat))
                  l3 (+ (* lr r4) (* lg g4) (* lb b4))
                  r5 (+ r4 (* (- l3 r4) bw))
                  g5 (+ g4 (* (- l3 g4) bw))
                  b5 (+ b4 (* (- l3 b4) bw))
                  v  (if (zero? vig)
                       1.0
                       (let [dx (* (- (/ (double x) width) 0.5) 2.0)
                             dy (* (- (/ (double y) height) 0.5) 2.0)]
                         (- 1.0 (* vig (max 0.0 (- (+ (* dx dx) (* dy dy)) 0.3)) 0.7))))
                  n  (if (zero? grain)
                       0.0
                       (let [q (* (Math/sin (+ (* (double x) 12.9898) (* (double y) 78.233))) 43758.5453)]
                         (* (- (- q (Math/floor q)) 0.5) grain 0.25)))
                  cr (max 0.0 (min 1.0 (+ (* r5 v) n)))
                  cg (max 0.0 (min 1.0 (+ (* g5 v) n)))
                  cb (max 0.0 (min 1.0 (+ (* b5 v) n)))]
              (aset out j       (float (scene/lut-at scene/srgb-decode-lut cr)))
              (aset out (+ j 1) (float (scene/lut-at scene/srgb-decode-lut cg)))
              (aset out (+ j 2) (float (scene/lut-at scene/srgb-decode-lut cb))))
            (recur (inc i))))))
    (scene/image width height out)))

;; --------------------------------------------------------------- geometry

(defn geometry-neutral?
  [settings]
  (every? (fn [k] (= (get settings k (defaults k)) (defaults k))) geometry-keys))

(defmacro ^:private sample-bilinear!
  "Writes the bilinear sample of float RGB `src` (row width `w`, height `h`) at
  (sx, sy) into `out` at float offset `o`, clamping at the edges."
  [src w h sx sy out o]
  `(let [sx#  ~sx sy# ~sy
         x0#  (Math/floor sx#) y0# (Math/floor sy#)
         fx#  (- sx# x0#)      fy# (- sy# y0#)
         xi#  (long x0#)       yi# (long y0#)
         xa#  (max 0 (min (dec ~w) xi#))   xb# (max 0 (min (dec ~w) (inc xi#)))
         ya#  (max 0 (min (dec ~h) yi#))   yb# (max 0 (min (dec ~h) (inc yi#)))
         i00# (* 3 (+ (* ya# ~w) xa#)) i10# (* 3 (+ (* ya# ~w) xb#))
         i01# (* 3 (+ (* yb# ~w) xa#)) i11# (* 3 (+ (* yb# ~w) xb#))]
     (dotimes [c# 3]
       (let [a# (double (aget ~src (+ i00# c#))) b# (double (aget ~src (+ i10# c#)))
             c0# (double (aget ~src (+ i01# c#))) d# (double (aget ~src (+ i11# c#)))]
         (aset ~out (+ ~o c#)
               (float (+ (* (- 1.0 fy#) (+ (* (- 1.0 fx#) a#) (* fx# b#)))
                         (* fy# (+ (* (- 1.0 fx#) c0#) (* fx# d#))))))))))

(defn geometry
  "Crop to the aspect ratio (centred), straighten by `angle` degrees and flip
  horizontally. Rotation zooms in just enough to keep the frame filled. Uses
  bilinear sampling in linear light; unchanged settings return the same image."
  [{:keys [^long width ^long height data] :as img} settings]
  (if (geometry-neutral? settings)
    img
    (let [iw    (double width)
          ih    (double height)
          aspect (get settings :aspect "orig")
          ar    (if (= aspect "orig") (/ iw ih) (double (aspect-ratios aspect (/ iw ih))))
          cw    (min iw (* ih ar))
          ch    (/ cw ar)
          w     (max 1 (Math/round cw))
          h     (max 1 (Math/round ch))
          th    (Math/toRadians (double (get settings :angle 0.0)))
          c     (Math/abs (Math/cos th))
          sn    (Math/abs (Math/sin th))
          z     (max 1.0 (/ (+ (* cw c) (* ch sn)) iw) (/ (+ (* cw sn) (* ch c)) ih))
          flip? (boolean (get settings :flip false))
          cos-t (Math/cos th)
          sin-t (Math/sin th)
          ^floats src data
          ^floats out (float-array (* 3 w h))]
      (core/parallel-ranges!
        (* w h)
        (fn [^long start ^long end]
          (loop [i start]
            (when (< i end)
              (let [x  (- (+ (rem i w) 0.5) (/ (double w) 2.0))
                    y  (- (+ (quot i w) 0.5) (/ (double h) 2.0))
                    ;; inverse of: rotate(th) then optional x-flip, then scale z
                    rx (+ (* x cos-t) (* y sin-t))
                    ry (- (* y cos-t) (* x sin-t))
                    rx (if flip? (- rx) rx)
                    sx (- (+ (/ rx z) (/ iw 2.0)) 0.5)
                    sy (- (+ (/ ry z) (/ ih 2.0)) 0.5)]
                (sample-bilinear! src width height sx sy out (* 3 i)))
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
                      sy (- (* (+ (quot i w) 0.5) (/ (double ph) h)) 0.5)]
                  (sample-bilinear! src pw ph sx sy out (* 3 i)))
                (recur (inc i))))))
        (scene/image w h out)))))
