(ns darkroom.imaging.develop
  "The Develop-view image engine: geometry (straighten / aspect crop / flip) and
  the tone pipeline (exposure, temperature, highlights/shadows, contrast, fade,
  tone curve, saturation, black & white, vignette, grain). A direct port of the
  design handoff's reference engine, so looks match the prototype. Pure logic,
  no UI dependency.

  Settings are a map; see `defaults`. Images are the usual
  {:width :height :pixels int-array} maps."
  (:require [darkroom.imaging.core :as core]))

(set! *unchecked-math* :warn-on-boxed)

(def default-curve [0.0 0.25 0.5 0.75 1.0])

(def defaults
  "Neutral values for every Develop setting."
  {:exposure 0.0 :contrast 0.0 :highlights 0.0 :shadows 0.0 :temp 0.0
   :saturation 0.0 :fade 0.0 :bw 0.0 :grain 0.0 :vignette 0.0
   :angle 0.0 :aspect "orig" :flip false :curve default-curve})

(def tone-keys
  [:exposure :contrast :highlights :shadows :temp :saturation :fade :bw :grain :vignette :curve])

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

(defn- byte-of
  "Channel value `c` plus grain `n`, clamped to 0-1 and scaled to 0-255 (round
  half to even, like a canvas Uint8ClampedArray)."
  ^long [^double c ^double n]
  (long (Math/rint (* 255.0 (max 0.0 (min 1.0 (+ c n)))))))

(defn tone
  "Applies the tone pipeline. Per pixel (floats 0-1), in order: exposure and
  temperature, shadows/highlights, contrast around 0.5, fade, tone curve,
  saturation, black & white mix, vignette, grain, clamp. Alpha is kept."
  [{:keys [^long width ^long height pixels]} settings]
  (let [ex      (Math/pow 2.0 (setting settings :exposure))
        con     (+ 1.0 (setting settings :contrast))
        sat     (+ 1.0 (setting settings :saturation))
        tm      (setting settings :temp)
        shd     (setting settings :shadows)
        hil     (setting settings :highlights)
        fade    (setting settings :fade)
        bw      (setting settings :bw)
        vig     (setting settings :vignette)
        grain   (setting settings :grain)
        ^doubles lut (curve-lut (get settings :curve default-curve))
        ^ints src pixels
        ^ints out (int-array (alength src))
        rgain   (* ex (+ 1.0 (* tm 0.2)))
        bgain   (* ex (- 1.0 (* tm 0.2)))]
    (core/parallel-ranges!
      (alength src)
      (fn [^long start ^long end]
        (loop [i start]
          (when (< i end)
            (let [p  (aget src (int i))
                  x  (rem i width)
                  y  (quot i width)
                  r0 (* (/ (bit-and (unsigned-bit-shift-right p 16) 0xFF) 255.0) rgain)
                  g0 (* (/ (bit-and (unsigned-bit-shift-right p 8) 0xFF) 255.0) ex)
                  b0 (* (/ (bit-and p 0xFF) 255.0) bgain)
                  l  (max 0.0 (min 1.0 (+ (* 0.2126 r0) (* 0.7152 g0) (* 0.0722 b0))))
                  sh (+ (* shd 0.35 (- 1.0 l) (- 1.0 l)) (* hil 0.35 l l))
                  r1 (+ (* (- (+ r0 sh) 0.5) con) 0.5)
                  g1 (+ (* (- (+ g0 sh) 0.5) con) 0.5)
                  b1 (+ (* (- (+ b0 sh) 0.5) con) 0.5)
                  k  (- 1.0 (* fade 0.25))
                  f2 (* fade 0.12)
                  r2 (+ (* r1 k) f2) g2 (+ (* g1 k) f2) b2 (+ (* b1 k) f2)
                  r3 (aget lut (int (* (max 0.0 (min 1.0 r2)) 255.0)))
                  g3 (aget lut (int (* (max 0.0 (min 1.0 g2)) 255.0)))
                  b3 (aget lut (int (* (max 0.0 (min 1.0 b2)) 255.0)))
                  l2 (+ (* 0.2126 r3) (* 0.7152 g3) (* 0.0722 b3))
                  r4 (+ l2 (* (- r3 l2) sat))
                  g4 (+ l2 (* (- g3 l2) sat))
                  b4 (+ l2 (* (- b3 l2) sat))
                  l3 (+ (* 0.2126 r4) (* 0.7152 g4) (* 0.0722 b4))
                  r5 (+ r4 (* (- l3 r4) bw))
                  g5 (+ g4 (* (- l3 g4) bw))
                  b5 (+ b4 (* (- l3 b4) bw))
                  v  (if (zero? vig)
                       1.0
                       (let [dx (* (- (/ (double x) width) 0.5) 2.0)
                             dy (* (- (/ (double y) height) 0.5) 2.0)]
                         (- 1.0 (* vig (max 0.0 (- (+ (* dx dx) (* dy dy)) 0.3)) 0.7))))
                  r6 (* r5 v) g6 (* g5 v) b6 (* b5 v)
                  n  (if (zero? grain)
                       0.0
                       (let [q (* (Math/sin (+ (* (double x) 12.9898) (* (double y) 78.233))) 43758.5453)]
                         (* (- (- q (Math/floor q)) 0.5) grain 0.25)))
                  ]
              (aset out (int i)
                    (unchecked-int (bit-or (bit-and p 0xFF000000)
                                           (bit-shift-left (byte-of r6 n) 16)
                                           (bit-shift-left (byte-of g6 n) 8)
                                           (byte-of b6 n)))))
            (recur (inc i))))))
    (core/image width height out)))

;; --------------------------------------------------------------- geometry

(defn- bilinear
  "Bilinear sample of packed-ARGB `src` (row width `w`) at (sx, sy), clamping at
  the edges. Returns packed ARGB with the nearest pixel's alpha."
  ^long [^ints src ^long w ^double sx ^double sy]
  (let [h   (quot (alength src) w)
        x0  (Math/floor sx) y0 (Math/floor sy)
        fx  (- sx x0)       fy (- sy y0)
        xi  (long x0)       yi (long y0)
        xa  (max 0 (min (dec w) xi))       xb (max 0 (min (dec w) (inc xi)))
        ya  (max 0 (min (dec h) yi))       yb (max 0 (min (dec h) (inc yi)))
        p00 (long (aget src (int (+ (* ya w) xa))))
        p10 (long (aget src (int (+ (* ya w) xb))))
        p01 (long (aget src (int (+ (* yb w) xa))))
        p11 (long (aget src (int (+ (* yb w) xb))))]
    (loop [s 16 acc (bit-and p00 0xFF000000)]
      (if (< s 0)
        acc
        (let [a (double (bit-and (unsigned-bit-shift-right p00 s) 0xFF))
              b (double (bit-and (unsigned-bit-shift-right p10 s) 0xFF))
              c (double (bit-and (unsigned-bit-shift-right p01 s) 0xFF))
              d (double (bit-and (unsigned-bit-shift-right p11 s) 0xFF))
              v (+ (* (- 1.0 fy) (+ (* (- 1.0 fx) a) (* fx b)))
                   (* fy (+ (* (- 1.0 fx) c) (* fx d))))]
          (recur (- s 8) (bit-or acc (bit-shift-left (Math/round v) s))))))))

(defn geometry-neutral?
  [settings]
  (every? (fn [k] (= (get settings k (defaults k)) (defaults k))) geometry-keys))

(defn geometry
  "Crop to the aspect ratio (centred), straighten by `angle` degrees and flip
  horizontally. Rotation zooms in just enough to keep the frame filled. Uses
  bilinear sampling; unchanged settings return the same image."
  [{:keys [^long width ^long height pixels] :as img} settings]
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
          ^ints src pixels
          ^ints out (int-array (* w h))
          iw*   width]
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
                (aset out (int i) (unchecked-int (bilinear src iw* sx sy))))
              (recur (inc i))))))
      (core/image w h out))))

;; ----------------------------------------------------------------- resize

(defn resize-long-edge
  "Scales `img` so its longest side is `edge` px (never enlarges). Large
  reductions box-average first (core/fit), then bilinear to the exact size."
  [{:keys [^long width ^long height] :as img} edge]
  (let [long-side (max width height)]
    (if (or (nil? edge) (<= (long edge) 0) (<= long-side (long edge)))
      img
      (let [pre (core/fit img (* 2 (long edge))) ; at most 2x too big: cheap box pass
            pw  (long (:width pre))
            ph  (long (:height pre))
            s   (/ (double (long edge)) (max pw ph))
            w   (max 1 (Math/round (* pw s)))
            h   (max 1 (Math/round (* ph s)))
            ^ints src (:pixels pre)
            ^ints out (int-array (* w h))]
        (core/parallel-ranges!
          (* w h)
          (fn [^long start ^long end]
            (loop [i start]
              (when (< i end)
                (let [sx (- (* (+ (rem i w) 0.5) (/ (double pw) w)) 0.5)
                      sy (- (* (+ (quot i w) 0.5) (/ (double ph) h)) 0.5)]
                  (aset out (int i) (unchecked-int (bilinear src pw sx sy))))
                (recur (inc i))))))
        (core/image w h out)))))
