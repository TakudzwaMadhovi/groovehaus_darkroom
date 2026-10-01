(ns darkroom.imaging.auto
  "Automatic adjustments: auto tone (exposure, contrast, highlights, shadows,
  whites, blacks) and white balance (from a picked colour, or estimated from the
  picture). They look at a small sample of a scene image and return settings;
  nothing here changes the image. Pure logic, no UI dependency."
  (:require [darkroom.imaging.color :as color]
            [darkroom.imaging.develop :as develop]
            [darkroom.imaging.scene :as scene]))

;; ------------------------------------------------------------------ sampling

(defn sample
  "A scene image of at most about `max-pixels` pixels, taken by striding over
  `img` (nearest pixel; fine for statistics, not for display)."
  [{:keys [^long width ^long height data] :as img} max-pixels]
  (let [k (long (max 1 (Math/ceil (Math/sqrt (/ (double (* width height)) (double max-pixels))))))]
    (if (= k 1)
      img
      (let [w (quot width k) h (quot height k)
            ^floats src data
            ^floats out (float-array (* 3 w h))]
        (dotimes [y h]
          (dotimes [x w]
            (let [s (* 3 (+ (* (* y k) width) (* x k))) o (* 3 (+ (* y w) x))]
              (aset out o (aget src s)) (aset out (+ o 1) (aget src (+ s 1))) (aset out (+ o 2) (aget src (+ s 2))))))
        (scene/image w h out)))))

(defn- luminances
  "Sorted working-space luminances (linear) of every pixel, as a double array."
  ^doubles [{:keys [data]}]
  (let [^floats src data
        [lr lg lb] (color/luma-weights :working)
        n (quot (alength src) 3)
        ^doubles out (double-array n)]
    (dotimes [i n]
      (let [j (* 3 i)]
        (aset out i (+ (* (double lr) (aget src j)) (* (double lg) (aget src (+ j 1))) (* (double lb) (aget src (+ j 2)))))))
    (java.util.Arrays/sort out)
    out))

(defn- percentile ^double [^doubles sorted p]
  (aget sorted (min (dec (alength sorted)) (long (* (double p) (alength sorted))))))

;; ----------------------------------------------------------------- white balance

(defn- residuals
  "[(R-G)/G (B-G)/G] of linear working colour `c` after the temperature/tint
  balance, seen in sRGB: zero when the colour comes out neutral."
  [c temp tint]
  (let [m (color/mat* (color/convert-matrix :working :srgb) (color/wb-matrix temp tint))
        [r g b] (color/mat-vec m c)
        g (max 1e-6 (double g))]
    [(/ (- (double r) g) g) (/ (- (double b) g) g)]))

(defn- error ^double [[a b]] (+ (* (double a) (double a)) (* (double b) (double b))))

(defn wb-from-color
  "{:temp :tint} (each -1..1) that make the linear working-space colour `rgb`
  neutral, found by Newton's method on the two colour-difference residuals
  (a colour the sliders cannot neutralise gets the closest the range allows)."
  [rgb]
  (let [c (mapv double rgb)
        eps 1e-4
        step (fn [t n]
               (let [[f1 f2] (residuals c t n)
                     [a1 a2] (residuals c (+ t eps) n)
                     [b1 b2] (residuals c t (+ n eps))
                     j11 (/ (- a1 f1) eps) j21 (/ (- a2 f2) eps)
                     j12 (/ (- b1 f1) eps) j22 (/ (- b2 f2) eps)
                     det (- (* j11 j22) (* j12 j21))]
                 (when (> (Math/abs det) 1e-12)
                   (let [dt (/ (- (* j22 f1) (* j12 f2)) det)
                         dn (/ (- (* j11 f2) (* j21 f1)) det)
                         lim (fn [d] (max -0.4 (min 0.4 d)))]
                     [(max -1.0 (min 1.0 (- t (lim dt)))) (max -1.0 (min 1.0 (- n (lim dn))))]))))
        [t n] (loop [t 0.0 n 0.0 i 0]
                (let [[t' n'] (or (step t n) [t n])]
                  (if (or (>= i 30) (and (< (Math/abs (- t' t)) 1e-7) (< (Math/abs (- n' n)) 1e-7)))
                    [t' n']
                    (recur t' n' (inc i)))))]
    {:temp (/ (Math/round (* t 1000.0)) 1000.0) :tint (/ (Math/round (* n 1000.0)) 1000.0)}))

(defn average-color
  "Mean linear working colour over the square of side `size` pixels centred on
  pixel (x, y), clamped to the image."
  [{:keys [^long width ^long height data]} x y size]
  (let [^floats src data
        r (quot (long size) 2)
        x0 (max 0 (- (long x) r)) x1 (min (dec width) (+ (long x) r))
        y0 (max 0 (- (long y) r)) y1 (min (dec height) (+ (long y) r))
        n (* (inc (- x1 x0)) (inc (- y1 y0)))
        sums (double-array 3)]
    (doseq [yy (range y0 (inc y1)) xx (range x0 (inc x1))]
      (let [j (* 3 (+ (* yy width) xx))]
        (dotimes [c 3] (aset sums c (+ (aget sums c) (aget src (+ j c)))))))
    (mapv #(/ (aget sums %) (double n)) (range 3))))

(defn auto-wb
  "{:temp :tint} estimated from the picture: the average colour of its
  mid-tone, not-very-colourful pixels (so a big green lawn does not drag the
  balance toward magenta), neutralised."
  [img]
  (let [{:keys [data]} (sample img 40000)
        ^floats src data
        [lr lg lb] (color/luma-weights :working)
        n (quot (alength src) 3)
        sums (double-array 3)
        used (loop [i 0 used 0]
               (if (< i n)
                 (let [j (* 3 i) r (aget src j) g (aget src (+ j 1)) b (aget src (+ j 2))
                       y (+ (* (double lr) r) (* (double lg) g) (* (double lb) b))
                       mx (max r g b) mn (min r g b)]
                   (if (and (< 0.04 y 0.85) (< mx 0.98) (pos? mx) (< (/ (- mx mn) mx) 0.45))
                     (do (aset sums 0 (+ (aget sums 0) r)) (aset sums 1 (+ (aget sums 1) g)) (aset sums 2 (+ (aget sums 2) b))
                         (recur (inc i) (inc used)))
                     (recur (inc i) used)))
                 used))]
    (if (< used (max 10 (quot n 100)))
      ;; (nearly) nothing neutral to go on: plain grey world
      (wb-from-color (mapv #(/ (reduce + (map (fn [i] (aget src (+ (* 3 i) %))) (range n))) (double n)) (range 3)))
      (wb-from-color (mapv #(/ (aget sums %) (double used)) (range 3))))))

;; ---------------------------------------------------------------------- auto tone

(def ^:private target-median
  "Median encoded luma an auto-toned picture is aimed at (about 18% grey in light)."
  0.46)

(defn- encoded-median
  "Median sRGB-encoded luma of `sampled` after the tone `settings`."
  ^double [sampled settings]
  (let [out (develop/tone sampled settings)
        l   (luminances out)]
    (scene/srgb-encode-extended (percentile l 0.5))))

(defn auto-tone
  "Tone settings for `img` {:exposure :contrast :highlights :shadows :whites
  :blacks}: exposure puts the median at mid grey, but never so high that more
  than a sliver clips; highlights, shadows, whites and blacks then pull in
  clipped or empty ends of the histogram; contrast flattens or boosts to a
  reasonable spread. `base` may carry the white balance (:temp :tint) the
  picture is being edited with, so the statistics are taken through it (the
  result holds only the six tone settings)."
  ([img] (auto-tone img {}))
  ([img base]
   (let [s       (sample img 40000)
         wb      (select-keys base [:temp :tint])
         seen    (luminances (develop/tone s wb))
         p01     (percentile seen 0.01) p05 (percentile seen 0.05) p50 (percentile seen 0.5)
         p95     (percentile seen 0.95) p995 (percentile seen 0.995)
         target  (scene/srgb-decode-extended target-median)
         ev0     (/ (Math/log (/ target (max 1e-4 p50))) (Math/log 2.0))
         ev-cap  (/ (Math/log (/ 1.3 (max 1e-4 p995))) (Math/log 2.0))
         ev      (max -2.0 (min 2.0 ev0 (max ev-cap -2.0)))
         ;; positions of the ends after exposure, in linear light
         hi      (* p995 (Math/pow 2.0 ev))
         lo      (* p01 (Math/pow 2.0 ev))
         dark    (* p05 (Math/pow 2.0 ev))
         clamp   (fn [lo hi v] (max lo (min hi v)))
         round2  (fn [v] (/ (Math/round (* 100.0 (double v))) 100.0))
         highlights (- (clamp 0.0 1.0 (/ (- hi 1.0) 0.6)))
         whites  (clamp -0.5 0.8 (/ (- 0.95 (min hi 1.0)) 0.4))
         shadows (* 0.6 (clamp 0.0 1.0 (/ (- 0.04 dark) 0.04)))
         blacks  (- (* 0.6 (clamp 0.0 1.0 (/ (- lo 0.004) 0.03))))
         enc-lo  (scene/srgb-encode-extended (* p05 (Math/pow 2.0 ev)))
         enc-hi  (scene/srgb-encode-extended (* p95 (Math/pow 2.0 ev)))
         contrast (clamp -0.3 0.5 (* 1.2 (- 0.62 (- enc-hi enc-lo))))
         shaped  (merge wb {:exposure ev :highlights highlights :shadows shadows :whites whites
                            :blacks blacks :contrast contrast})
         ;; the other controls move the median a little: take it back to the target
         ev2     (loop [ev ev i 0]
                   (if (= i 3)
                     ev
                     (let [m (encoded-median s (assoc shaped :exposure ev))
                           err (- target-median m)]
                       (if (< (Math/abs err) 0.01)
                         ev
                         (recur (clamp -2.0 (max -2.0 (min 2.0 ev-cap)) (+ ev (* 3.0 err))) (inc i))))))]
     {:exposure (round2 ev2) :contrast (round2 contrast) :highlights (round2 highlights)
      :shadows (round2 shadows) :whites (round2 whites) :blacks (round2 blacks)})))
