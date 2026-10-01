(ns darkroom.imaging.develop
  "The Develop-view tone engine on float scene images (darkroom.imaging.scene):
  the tone pipeline and the tone curve. (Geometry and resize live in
  darkroom.imaging.geometry.) Pure logic, no UI dependency.

  Exposure and white balance act in linear light, where they are physically
  meaningful. The remaining tone steps (shadows/highlights, contrast, fade,
  curve, saturation, black & white, vignette, grain) run on the sRGB-encoded
  (perceptual) values of the same pixel, as in the original design engine; those
  values may exceed 1 so highlights can still be recovered, and are clamped only
  at the end.

  Settings are a map; see `defaults`."
  (:require [darkroom.imaging.color :as color]
            [darkroom.imaging.core :as core]
            [darkroom.imaging.geometry :as geometry]
            [darkroom.imaging.scene :as scene]))

(set! *unchecked-math* :warn-on-boxed)

(def default-curve [0.0 0.25 0.5 0.75 1.0])

(def hsl-bands
  "The HSL mixer's colour bands: [name centre-hue-in-degrees]."
  [["RED" 0] ["ORANGE" 30] ["YELLOW" 60] ["GREEN" 120] ["AQUA" 180] ["BLUE" 240] ["PURPLE" 270] ["MAGENTA" 300]])

(def default-hsl
  "[hue saturation luminance] per band, each -1..1."
  (vec (repeat (count hsl-bands) [0.0 0.0 0.0])))

(def defaults
  "Neutral values for every Develop setting (tone here, geometry from
  darkroom.imaging.geometry)."
  (merge
    geometry/defaults
    {:exposure 0.0 :contrast 0.0 :highlights 0.0 :shadows 0.0 :whites 0.0 :blacks 0.0
     :temp 0.0 :tint 0.0 :vibrance 0.0 :saturation 0.0 :hsl default-hsl
     :split-sh-hue 220.0 :split-sh-sat 0.0 :split-hl-hue 40.0 :split-hl-sat 0.0 :split-balance 0.0
     :fade 0.0 :bw 0.0 :grain 0.0 :vignette 0.0
     :curve default-curve :curve-r default-curve :curve-g default-curve :curve-b default-curve}))

(def tone-keys
  [:exposure :contrast :highlights :shadows :whites :blacks :temp :tint :vibrance :saturation :hsl
   :split-sh-hue :split-sh-sat :split-hl-hue :split-hl-sat :split-balance
   :fade :bw :grain :vignette :curve :curve-r :curve-g :curve-b])

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

(defn- fmod
  "x modulo m, always in [0, m) (unlike rem)."
  ^double [^double x ^double m]
  (- x (* m (Math/floor (/ x m)))))

(defn- hsl-lut
  "360 x 3 table for the HSL mixer: for each whole hue degree, the hue shift
  (degrees), saturation and lightness amounts blended between the two nearest
  band centres."
  ^doubles [hsl]
  (let [out     (double-array (* 3 360))
        centres (mapv second hsl-bands)
        n       (long (count centres))]
    (dotimes [d 360]
      (let [i  (long (loop [k (dec n)] (if (>= d (long (nth centres k))) k (recur (dec k)))))
            j  (long (mod (inc i) n))
            ci (double (nth centres i))
            cj (double (if (zero? j) 360 (nth centres j)))
            t  (/ (- (double d) ci) (- cj ci))
            [h1 s1 l1] (nth hsl i)
            [h2 s2 l2] (nth hsl j)
            mix (fn [a b] (+ (* (- 1.0 t) (double a)) (* t (double b))))]
        (aset out (* 3 d) (* 30.0 (double (mix h1 h2))))
        (aset out (+ (* 3 d) 1) (double (mix s1 s2)))
        (aset out (+ (* 3 d) 2) (double (mix l1 l2)))))
    out))

(defn- hsl-adjust!
  "HSL mixer on the encoded colour in io[0..2] (each 0-1), using `lut` from
  hsl-lut. Greys are untouched and the effect fades in with chroma. Result is
  written back to io."
  [^doubles lut ^doubles io]
  (let [r (aget io 0) g (aget io 1) b (aget io 2)
        mx (max r (max g b)) mn (min r (min g b)) d (- mx mn)
        l  (* 0.5 (+ mx mn))
        den (- 1.0 (Math/abs (- (* 2.0 l) 1.0)))]
    (when (and (> d 1e-6) (> den 1e-6))
      (let [s    (/ d den)
            h    (* 60.0 (double (cond (== mx r) (fmod (/ (- g b) d) 6.0)
                                   (== mx g) (+ 2.0 (/ (- b r) d))
                                   :else     (+ 4.0 (/ (- r g) d)))))
            idx  (* 3 (min 359 (long h)))
            gate (min 1.0 (* 5.0 d))
            h2   (fmod (+ h (* gate (aget lut idx))) 360.0)
            s2   (max 0.0 (min 1.0 (* s (+ 1.0 (* gate (aget lut (+ idx 1)))))))
            l2   (max 0.0 (min 1.0 (+ l (* gate 0.25 (aget lut (+ idx 2))))))
            c    (* (- 1.0 (Math/abs (- (* 2.0 l2) 1.0))) s2)
            hp   (/ h2 60.0)
            x    (* c (- 1.0 (Math/abs (- (fmod hp 2.0) 1.0))))
            m    (- l2 (* 0.5 c))]
        (case (long hp)
          0 (do (aset io 0 (+ c m)) (aset io 1 (+ x m)) (aset io 2 m))
          1 (do (aset io 0 (+ x m)) (aset io 1 (+ c m)) (aset io 2 m))
          2 (do (aset io 0 m) (aset io 1 (+ c m)) (aset io 2 (+ x m)))
          3 (do (aset io 0 m) (aset io 1 (+ x m)) (aset io 2 (+ c m)))
          4 (do (aset io 0 (+ x m)) (aset io 1 m) (aset io 2 (+ c m)))
          (do (aset io 0 (+ c m)) (aset io 1 m) (aset io 2 (+ x m))))))))

(defn- hue-offset
  "Chroma-only colour offset for a split-tone hue (degrees) and strength 0-1:
  the full-saturation hue minus its own luma, so toning shifts colour without
  shifting brightness. Returns a 3-element double array."
  ^doubles [^double hue ^double sat]
  (let [hp (/ (fmod hue 360.0) 60.0)
        x  (- 1.0 (Math/abs (- (fmod hp 2.0) 1.0)))
        ^doubles rgb (double-array (case (long hp) 0 [1.0 x 0.0] 1 [x 1.0 0.0] 2 [0.0 1.0 x] 3 [0.0 x 1.0] 4 [x 0.0 1.0] [1.0 0.0 x]))
        ^doubles lw  (double-array working-luma)
        r (aget rgb 0) g (aget rgb 1) b (aget rgb 2)
        y (+ (* (aget lw 0) r) (* (aget lw 1) g) (* (aget lw 2) b))]
    (double-array [(* 0.5 sat (- r y)) (* 0.5 sat (- g y)) (* 0.5 sat (- b y))])))

(defn- smoothstep ^double [^double x]
  (let [t (max 0.0 (min 1.0 x))] (* t t (- 3.0 (* 2.0 t)))))

(defn- endpoints
  "White/black point move on an encoded value: whites scale the upper range
  (negligible in the shadows), blacks shift the lower range (negligible in the
  highlights). +whites brightens highlights, +blacks lifts blacks."
  ^double [^double v ^double whites ^double blacks]
  (+ (* v (+ 1.0 (* 0.2 whites (smoothstep (/ (- v 0.25) 0.75)))))
     (* 0.12 blacks (- 1.0 (smoothstep (/ v 0.4))))))

(defn tone
  "Applies the tone pipeline to a scene image and returns a new one. Per pixel:
  white balance and exposure (linear light); then, on the encoded values, white
  and black points, shadows/highlights, contrast around 0.5, fade, tone curves
  (master, then per channel), the HSL mixer, saturation and vibrance, split
  toning, black & white mix, vignette, grain; clamp to [0, 1] and back to linear."
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
        vib     (setting settings :vibrance)
        shd     (setting settings :shadows)
        hil     (setting settings :highlights)
        whites  (setting settings :whites)
        blacks  (setting settings :blacks)
        ends?   (or (not (zero? whites)) (not (zero? blacks)))
        fade    (setting settings :fade)
        bw      (setting settings :bw)
        vig     (setting settings :vignette)
        grain   (setting settings :grain)
        ^doubles lut  (curve-lut (get settings :curve default-curve))
        ^doubles lutr (curve-lut (get settings :curve-r default-curve))
        ^doubles lutg (curve-lut (get settings :curve-g default-curve))
        ^doubles lutb (curve-lut (get settings :curve-b default-curve))
        hsl     (get settings :hsl default-hsl)
        hsl?    (not= hsl default-hsl)
        ^doubles hlut (hsl-lut hsl)
        sh-sat  (setting settings :split-sh-sat)
        hl-sat  (setting settings :split-hl-sat)
        split?  (or (pos? sh-sat) (pos? hl-sat))
        ^doubles sh-off (hue-offset (setting settings :split-sh-hue) sh-sat)
        ^doubles hl-off (hue-offset (setting settings :split-hl-hue) hl-sat)
        sh-r (aget sh-off 0) sh-g (aget sh-off 1) sh-b (aget sh-off 2)
        hl-r (aget hl-off 0) hl-g (aget hl-off 1) hl-b (aget hl-off 2)
        pivot   (+ 0.5 (* 0.3 (setting settings :split-balance)))
        ^floats src data
        ^floats out (float-array (alength src))
        k       (- 1.0 (* fade 0.25))
        f2      (* fade 0.12)]
    (core/parallel-ranges!
      (quot (alength src) 3)
      (fn [^long start ^long end]
        (let [^doubles io (double-array 3)]
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
                    ;; perceptual domain, with the white/black points applied
                    er (scene/srgb-encode-extended r0)
                    eg (scene/srgb-encode-extended g0)
                    eb (scene/srgb-encode-extended b0)
                    er (if ends? (endpoints er whites blacks) er)
                    eg (if ends? (endpoints eg whites blacks) eg)
                    eb (if ends? (endpoints eb whites blacks) eb)
                    l  (max 0.0 (min 1.0 (scene/srgb-encode-extended (+ (* lr r0) (* lg g0) (* lb b0)))))
                    sh (+ (* shd 0.35 (- 1.0 l) (- 1.0 l)) (* hil 0.35 l l))
                    r1 (+ (* (- (+ er sh) 0.5) con) 0.5)
                    g1 (+ (* (- (+ eg sh) 0.5) con) 0.5)
                    b1 (+ (* (- (+ eb sh) 0.5) con) 0.5)
                    r2 (+ (* r1 k) f2) g2 (+ (* g1 k) f2) b2 (+ (* b1 k) f2)
                    r3 (scene/lut-at lutr (scene/lut-at lut r2))
                    g3 (scene/lut-at lutg (scene/lut-at lut g2))
                    b3 (scene/lut-at lutb (scene/lut-at lut b2))
                    ;; HSL mixer (needs 0-1 input)
                    _  (when hsl?
                         (aset io 0 (max 0.0 (min 1.0 r3))) (aset io 1 (max 0.0 (min 1.0 g3))) (aset io 2 (max 0.0 (min 1.0 b3)))
                         (hsl-adjust! hlut io))
                    r3 (if hsl? (aget io 0) r3)
                    g3 (if hsl? (aget io 1) g3)
                    b3 (if hsl? (aget io 2) b3)
                    l2 (+ (* lr r3) (* lg g3) (* lb b3))
                    r4 (+ l2 (* (- r3 l2) sat))
                    g4 (+ l2 (* (- g3 l2) sat))
                    b4 (+ l2 (* (- b3 l2) sat))
                    ;; vibrance: lift muted colours more than saturated ones, and
                    ;; ease off on skin-tone orange (r > g > b, hue roughly 15-45 degrees)
                    ;; (the working space is wider than sRGB: scale its chroma so an
                    ;; sRGB-saturated colour counts as saturated)
                    chroma (* 1.6 (- (max r4 (max g4 b4)) (min r4 (min g4 b4))))
                    skin?  (and (> r4 g4) (> g4 b4) (> chroma 1e-6)
                                (let [t (/ (- g4 b4) (- r4 b4))] (and (> t 0.25) (< t 0.75))))
                    vk     (if (pos? vib)
                             (* vib (let [w (- 1.0 (min 1.0 chroma))] (* w w)) (if skin? 0.5 1.0))
                             vib)
                    l3 (+ (* lr r4) (* lg g4) (* lb b4))
                    r4v (+ l3 (* (- r4 l3) (+ 1.0 vk)))
                    g4v (+ l3 (* (- g4 l3) (+ 1.0 vk)))
                    b4v (+ l3 (* (- b4 l3) (+ 1.0 vk)))
                    ;; split toning
                    lt  (max 0.0 (min 1.0 (+ (* lr r4v) (* lg g4v) (* lb b4v))))
                    wsh (if split? (smoothstep (/ (- pivot lt) pivot)) 0.0)
                    whl (if split? (smoothstep (/ (- lt pivot) (- 1.0 pivot))) 0.0)
                    r4s (+ r4v (* wsh sh-r) (* whl hl-r))
                    g4s (+ g4v (* wsh sh-g) (* whl hl-g))
                    b4s (+ b4v (* wsh sh-b) (* whl hl-b))
                    ;; keep the pixel's brightness: toning moves colour only
                    dl  (if split? (- (+ (* lr r4v) (* lg g4v) (* lb b4v)) (+ (* lr r4s) (* lg g4s) (* lb b4s))) 0.0)
                    r4t (+ r4s dl) g4t (+ g4s dl) b4t (+ b4s dl)
                    l4 (+ (* lr r4t) (* lg g4t) (* lb b4t))
                    r5 (+ r4t (* (- l4 r4t) bw))
                    g5 (+ g4t (* (- l4 g4t) bw))
                    b5 (+ b4t (* (- l4 b4t) bw))
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
              (recur (inc i)))))))
    (scene/image width height out)))
