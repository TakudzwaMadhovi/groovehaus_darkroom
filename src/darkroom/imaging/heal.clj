(ns darkroom.imaging.heal
  "Spot removal: clone (copy a patch from elsewhere, feathered) and heal (copy a
  patch and blend it into its surroundings with mean-value cloning, so colour and
  brightness match). The source patch is chosen automatically as the nearby
  area whose surroundings best match the spot's. Spots are positioned on the
  image being edited as fractions of its size, with the radius a fraction of the
  long edge. Pure logic, no UI dependency."
  (:require [darkroom.imaging.scene :as scene]))

(def defaults {:spots []})

(defn spots-neutral? [settings] (empty? (get settings :spots)))

(defn- smoothstep ^double [^double x]
  (let [t (max 0.0 (min 1.0 x))] (* t t (- 3.0 (* 2.0 t)))))

(defn- radius-px ^double [{:keys [r]} ^long w ^long h]
  (max 3.0 (* (double r) (double (max w h)))))

;; ------------------------------------------------------------ source choice

(defn- ring-stats
  "[mean-r mean-g mean-b mean-abs-gradient] over the annulus r..1.7r around
  (cx, cy), skipping pixels outside the image; nil if too few are inside."
  [{:keys [^long width ^long height data]} ^double cx ^double cy ^double r]
  (let [^floats d data
        r2 (* r 1.7) n (atom 0) sums (double-array 4)
        x0 (max 1 (long (- cx r2))) x1 (min (- width 2) (long (+ cx r2)))
        y0 (max 1 (long (- cy r2))) y1 (min (- height 2) (long (+ cy r2)))]
    (loop [y y0]
      (when (<= y y1)
        (loop [x x0]
          (when (<= x x1)
            (let [dd (Math/hypot (- (+ x 0.5) cx) (- (+ y 0.5) cy))]
              (when (and (>= dd r) (<= dd r2))
                (let [i (* 3 (+ (* y width) x)) l (* 3 (+ (* y width) x 1)) b (* 3 (+ (* (inc y) width) x))]
                  (swap! n inc)
                  (dotimes [c 3] (aset sums c (+ (aget sums c) (aget d (+ i c)))))
                  (aset sums 3 (+ (aget sums 3) (Math/abs (- (aget d i) (aget d l))) (Math/abs (- (aget d i) (aget d b)))))))
              )
            (recur (inc x))))
        (recur (inc y))))
    (when (> @n 20)
      (let [k (double @n)] [(/ (aget sums 0) k) (/ (aget sums 1) k) (/ (aget sums 2) k) (/ (aget sums 3) k)]))))

(defn auto-source
  "The centre (px, py), in pixels, of the patch to copy over a spot at (x, y)
  with radius r (pixels): of the candidates around it (2.5 and 4 radii, eight
  directions), the one whose surroundings are most alike in colour and texture
  and which does not overlap the spot."
  [img x y r]
  (let [{:keys [^long width ^long height]} img
        target (ring-stats img x y r)
        cands  (for [dist [2.5 4.0] k (range 8)
                     :let [a (* k (/ Math/PI 4.0))
                           cx (+ (double x) (* dist (double r) (Math/cos a)))
                           cy (+ (double y) (* dist (double r) (Math/sin a)))]
                     :when (and (> cx (* 1.7 r)) (< cx (- width (* 1.7 r))) (> cy (* 1.7 r)) (< cy (- height (* 1.7 r))))]
                 [cx cy])
        score  (fn [[cx cy]]
                 (when-let [s (ring-stats img cx cy r)]
                   (if target
                     (reduce + (map (fn [a b] (let [d (- (double a) (double b))] (* d d))) (take 3 s) (take 3 target)))
                     0.0)))
        scored (remove #(nil? (second %)) (map (fn [c] [c (score c)]) cands))]
    (if (seq scored)
      (first (apply min-key second scored))
      ;; boxed in: any point clear of the spot
      [(if (> (double x) (/ (double width) 2)) (max (* 1.0 r) (- (double x) (* 2.5 r))) (min (- width (* 1.0 r)) (+ (double x) (* 2.5 r)))) (double y)])))

;; Both operations read the picture as it was before the spot, but must not copy
;; the whole image per spot: they snapshot just the spot's bounding box.

(defn- copy-box
  "A copy of the box [x0..x1] x [y0..y1] of float RGB `d` (row width `width`)."
  ^floats [^floats d width x0 y0 x1 y1]
  (let [width (long width) x0 (long x0) y0 (long y0) x1 (long x1) y1 (long y1)
        bw (inc (- x1 x0)) bh (inc (- y1 y0))
        out (float-array (* 3 bw bh))]
    (dotimes [yy bh]
      (System/arraycopy d (* 3 (+ (* (+ y0 yy) width) x0)) out (* 3 yy bw) (* 3 bw)))
    out))

;; ------------------------------------------------------------------ clone

(defn- clone-spot!
  "Feathered copy of the patch at (sx, sy) onto the spot at (cx, cy), in place."
  [^floats out width height cx cy sx sy r feather opacity]
  (let [width (long width) height (long height)
        cx (double cx) cy (double cy) r (double r) f (max 0.01 (double feather)) op (double opacity)
        dx (- (double sx) cx) dy (- (double sy) cy)
        x0 (max 0 (long (- cx r))) x1 (min (dec width) (long (+ cx r)))
        y0 (max 0 (long (- cy r))) y1 (min (dec height) (long (+ cy r)))
        bw (inc (- x1 x0))
        ^floats box (copy-box out width x0 y0 x1 y1)]
    (loop [y y0]
      (when (<= y y1)
        (loop [x x0]
          (when (<= x x1)
            (let [d (/ (Math/hypot (- (+ x 0.5) cx) (- (+ y 0.5) cy)) r)]
              (when (< d 1.0)
                (let [a (* op (- 1.0 (smoothstep (/ (- d (- 1.0 f)) f))))
                      sx' (min (dec width) (max 0 (long (Math/round (+ x dx)))))
                      sy' (min (dec height) (max 0 (long (Math/round (+ y dy)))))
                      i (* 3 (+ (* y width) x))
                      in-box? (and (<= x0 sx' x1) (<= y0 sy' y1))
                      ^floats sarr (if in-box? box out)
                      j (if in-box? (* 3 (+ (* (- sy' y0) bw) (- sx' x0))) (* 3 (+ (* sy' width) sx')))]
                  (dotimes [c 3]
                    (aset out (+ i c) (float (+ (* (- 1.0 a) (aget out (+ i c))) (* a (aget sarr (+ j c))))))))))
            (recur (inc x))))
        (recur (inc y))))))

;; ------------------------------------------------------------------- heal

(def ^:private boundary-points 64)

(defn- sample-encoded
  "Bilinear sample of float RGB `d` at (x, y), sRGB-encoded, written into
  `out3` (3 doubles); coordinates are clamped into the image."
  [^floats d width height x y ^doubles out3]
  (let [width (long width) height (long height) x (double x) y (double y)
        px (- x 0.5) py (- y 0.5)
        x0 (Math/floor px) y0 (Math/floor py)
        fx (- px x0) fy (- py y0)
        xa (max 0 (min (dec width) (long x0))) xb (max 0 (min (dec width) (inc (long x0))))
        ya (max 0 (min (dec height) (long y0))) yb (max 0 (min (dec height) (inc (long y0))))]
    (dotimes [c 3]
      (let [a (aget d (+ (* 3 (+ (* ya width) xa)) c)) b (aget d (+ (* 3 (+ (* ya width) xb)) c))
            e (aget d (+ (* 3 (+ (* yb width) xa)) c)) f (aget d (+ (* 3 (+ (* yb width) xb)) c))
            v (+ (* (- 1.0 fy) (+ (* (- 1.0 fx) a) (* fx b))) (* fy (+ (* (- 1.0 fx) e) (* fx f))))]
        (aset out3 c (scene/srgb-encode-extended v))))))

(defn- mvc-correction!
  "Writes into `acc` (3 doubles) the colour correction at (px, py), interpolated
  from the boundary mismatch `diff` with mean-value coordinates over the polygon
  (bx, by)."
  [^doubles bx ^doubles by ^doubles diff ^doubles acc px py]
  (let [px (double px) py (double py)
        k (alength bx)]
    (java.util.Arrays/fill acc 0.0)
    (loop [i 0 wsum 0.0]
      (if (< i k)
        (let [j (rem (inc i) k) p (rem (+ i (dec k)) k)
              vx (- (aget bx i) px) vy (- (aget by i) py)
              di (Math/sqrt (+ (* vx vx) (* vy vy)))]
          (if (< di 1e-6)
            ;; on a vertex: its own mismatch
            (dotimes [c 3] (aset acc c (aget diff (+ (* 3 i) c))))
            (let [ux (- (aget bx j) px) uy (- (aget by j) py)
                  wx (- (aget bx p) px) wy (- (aget by p) py)
                  dj (Math/sqrt (+ (* ux ux) (* uy uy)))
                  dp (Math/sqrt (+ (* wx wx) (* wy wy)))
                  ;; tan(alpha/2) = cross / (|a||b| + dot)
                  t-next (/ (- (* vx uy) (* vy ux)) (+ (* di dj) (* vx ux) (* vy uy)))
                  t-prev (/ (- (* wx vy) (* wy vx)) (+ (* dp di) (* wx vx) (* wy vy)))
                  w (/ (+ t-prev t-next) di)]
              (dotimes [c 3] (aset acc c (+ (aget acc c) (* w (aget diff (+ (* 3 i) c))))))
              (recur (inc i) (+ wsum w)))))
        ;; normalise (unless a vertex case already wrote the final value)
        (when (> (Math/abs wsum) 1e-12)
          (dotimes [c 3] (aset acc c (/ (aget acc c) wsum))))))))

(defn- heal-spot!
  "Heals the spot at (cx, cy): the patch at (sx, sy) is copied in and its
  colour mismatch with the surroundings, measured on the spot's boundary, is
  spread smoothly over it with mean-value coordinates (Farbman et al.), so the
  join is seamless. Works on sRGB-encoded values, in place."
  [^floats out width height cx cy sx sy r feather opacity]
  (let [width (long width) height (long height)
        cx (double cx) cy (double cy) r (double r)
        ox (- (double sx) cx) oy (- (double sy) cy)
        f (max 0.01 (double feather)) op (double opacity)
        k (long boundary-points)
        ^doubles bx (double-array k) ^doubles by (double-array k)
        ^doubles diff (double-array (* 3 k))
        ^doubles s1 (double-array 3) ^doubles s2 (double-array 3)]
    ;; the mismatch (destination - source) around the circle
    (dotimes [i k]
      (let [t (* 2.0 Math/PI (/ (double i) k))
            x (+ cx (* r (Math/cos t))) y (+ cy (* r (Math/sin t)))]
        (aset bx i x) (aset by i y)
        (sample-encoded out width height x y s1)
        (sample-encoded out width height (+ x ox) (+ y oy) s2)
        (dotimes [c 3] (aset diff (+ (* 3 i) c) (- (aget s1 c) (aget s2 c))))))
    (let [x0 (max 0 (long (- cx r))) x1 (min (dec width) (long (+ cx r)))
          y0 (max 0 (long (- cy r))) y1 (min (dec height) (long (+ cy r)))
          bw (inc (- x1 x0))
          ^floats box (copy-box out width x0 y0 x1 y1)
          ^doubles acc (double-array 3)]
      (loop [y y0]
        (when (<= y y1)
          (loop [x x0]
            (when (<= x x1)
              (let [px (+ x 0.5) py (+ y 0.5)
                    d (/ (Math/hypot (- px cx) (- py cy)) r)]
                (when (< d 1.0)
                  (mvc-correction! bx by diff acc px py)
                  (let [sxp (min (dec width) (max 0 (long (Math/floor (+ px ox)))))
                        syp (min (dec height) (max 0 (long (Math/floor (+ py oy)))))
                        o (* 3 (+ (* y width) x))
                        in-box? (and (<= x0 sxp x1) (<= y0 syp y1))
                        ^floats sarr (if in-box? box out)
                        j (if in-box? (* 3 (+ (* (- syp y0) bw) (- sxp x0))) (* 3 (+ (* syp width) sxp)))
                        a (* op (- 1.0 (smoothstep (/ (- d (- 1.0 f)) f))))]
                    (dotimes [c 3]
                      (let [healed (scene/srgb-decode-extended
                                     (+ (scene/srgb-encode-extended (aget sarr (+ j c))) (aget acc c)))]
                        (aset out (+ o c) (float (+ (* (- 1.0 a) (aget out (+ o c))) (* a healed)))))))))
              (recur (inc x))))
          (recur (inc y)))))))

(defn apply-spots
  "Applies each spot {:x :y :r :mode (:heal or :clone) [:sx :sy] :feather :opacity}
  of (:spots settings) to a scene image, in order, and returns a new image. Spots
  without a source (fractions :sx :sy) get one from `auto-source`."
  [{:keys [^long width ^long height data] :as img} settings]
  (if (spots-neutral? settings)
    img
    (let [^floats out (aclone ^floats data)]
      (doseq [{:keys [x y mode sx sy feather opacity] :as spot} (:spots settings)]
        (let [cx (* (double x) width) cy (* (double y) height)
              r  (radius-px spot width height)
              cur (scene/image width height out)
              [px py] (if (and sx sy) [(* (double sx) width) (* (double sy) height)] (auto-source cur cx cy r))
              f  (or feather 0.4) op (or opacity 1.0)]
          (if (= mode :clone)
            (clone-spot! out width height cx cy px py r f op)
            (heal-spot! out width height cx cy px py r f op))))
      (scene/image width height out))))
