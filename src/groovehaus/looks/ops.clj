(ns groovehaus.looks.ops
  "Primitive in-place pixel operations on a working buffer. Look render fns compose these."
  (:require [groovehaus.looks.buffer :as buf]))

(set! *warn-on-reflection* true)

(defn- clamp01 ^double [^double v] (Math/max 0.0 (Math/min 1.0 v)))

(defn smoothstep ^double [^double e0 ^double e1 ^double x]
  (let [t (clamp01 (/ (- x e0) (- e1 e0)))]
    (* t t (- 3.0 (* 2.0 t)))))

(defn s-curve
  "Contrast curve: v blended toward smoothstep(v) by k (k<0 flattens, k>1 exaggerates)."
  ^double [^double v ^double k]
  (+ v (* k (- (smoothstep 0.0 1.0 v) v))))

(defn luma ^double [^floats rgb ^long i]
  (+ (* 0.2126 (aget rgb i)) (* 0.7152 (aget rgb (+ i 1))) (* 0.0722 (aget rgb (+ i 2)))))

;; ---- LUT based tone ops -------------------------------------------------

(def ^:private ^:const lut-size 1024)

(defn make-lut ^floats [f]
  (let [a (float-array (inc lut-size))]
    (dotimes [i (inc lut-size)]
      (aset a i (float (clamp01 (double (f (/ i (double lut-size))))))))
    a))

(defn- lut-lookup ^double [^floats lut ^double v]
  (let [p (* (clamp01 v) lut-size)
        i (long p)
        i1 (Math/min (inc i) (long lut-size))
        t (- p i)]
    (+ (* (- 1.0 t) (aget lut i)) (* t (aget lut i1)))))

(defn curves!
  "Applies a separate curve (fn of 0..1 -> 0..1) to each channel."
  [buf fr fg fb]
  (let [{:keys [^long w ^long h ^floats rgb]} buf
        lr (make-lut fr) lg (make-lut fg) lb (make-lut fb)]
    (buf/par-rows h
      (fn [^long y]
        (dotimes [x w]
          (let [i (* 3 (+ (* y w) x))]
            (aset rgb i (float (lut-lookup lr (aget rgb i))))
            (aset rgb (+ i 1) (float (lut-lookup lg (aget rgb (+ i 1)))))
            (aset rgb (+ i 2) (float (lut-lookup lb (aget rgb (+ i 2)))))))))
    buf))

(defn tone! [buf f] (curves! buf f f f))

(defn contrast!
  "amount in [-1,1]; 0 is identity."
  [buf ^double amount]
  (if (zero? amount) buf (tone! buf (fn [v] (s-curve v amount)))))

(defn fade!
  "Matte look: lifts blacks up to 25% and pulls whites down up to 8% at amount=1."
  [buf ^double amount]
  (if (zero? amount)
    buf
    (let [lo (* 0.25 amount) hi (- 1.0 (* 0.08 amount))]
      (tone! buf (fn [v] (+ lo (* v (- hi lo))))))))

(defn grayscale! [buf]
  (let [{:keys [^long w ^long h ^floats rgb]} buf]
    (buf/par-rows h
      (fn [^long y]
        (dotimes [x w]
          (let [i (* 3 (+ (* y w) x))
                l (float (luma rgb i))]
            (aset rgb i l) (aset rgb (+ i 1) l) (aset rgb (+ i 2) l)))))
    buf))

(defn duotone!
  "Maps luma onto a gradient from `shadow` to `highlight` ([r g b] in 0..1)."
  [buf [^double sr ^double sg ^double sb] [^double hr ^double hg ^double hb]]
  (let [{:keys [^long w ^long h ^floats rgb]} buf]
    (buf/par-rows h
      (fn [^long y]
        (dotimes [x w]
          (let [i (* 3 (+ (* y w) x))
                t (luma rgb i)]
            (aset rgb i (float (+ sr (* t (- hr sr)))))
            (aset rgb (+ i 1) (float (+ sg (* t (- hg sg)))))
            (aset rgb (+ i 2) (float (+ sb (* t (- hb sb)))))))))
    buf))

;; ---- vignette -----------------------------------------------------------

(defn vignette!
  "Darkens toward the corners. `radius` (0..1) is the normalised distance from centre
  (0 = centre, 1 = corner) where darkening begins; `strength` is the darkening at the corner."
  [buf ^double radius ^double strength]
  (let [{:keys [^long w ^long h ^floats rgb]} buf
        cx (/ w 2.0) cy (/ h 2.0)
        diag (Math/sqrt (+ (* cx cx) (* cy cy)))
        span (Math/max 1e-3 (- 1.0 radius))]
    (when (pos? strength)
      (buf/par-rows h
        (fn [^long y]
          (let [dy (- (+ y 0.5) cy)]
            (dotimes [x w]
              (let [dx (- (+ x 0.5) cx)
                    d (/ (Math/sqrt (+ (* dx dx) (* dy dy))) diag)
                    t (clamp01 (/ (- d radius) span))
                    f (- 1.0 (* strength t t (- 3.0 (* 2.0 t))))
                    i (* 3 (+ (* y w) x))]
                (aset rgb i (float (* f (aget rgb i))))
                (aset rgb (+ i 1) (float (* f (aget rgb (+ i 1)))))
                (aset rgb (+ i 2) (float (* f (aget rgb (+ i 2)))))))))))
    buf))

;; ---- grain ----------------------------------------------------------------
;; Counter-based noise: value depends only on (cell x, cell y, seed, salt), so renders are
;; deterministic, thread-order independent, and identical between preview and export.

(defn- mix64 ^long [^long z]
  (let [z (unchecked-multiply (bit-xor z (unsigned-bit-shift-right z 33)) -49064778989728563)
        z (unchecked-multiply (bit-xor z (unsigned-bit-shift-right z 33)) -4265267296055464877)]
    (bit-xor z (unsigned-bit-shift-right z 33))))

(defn- uniform ^double [^long x ^long y ^long seed ^long salt]
  (let [k (mix64 (unchecked-add x (mix64 (unchecked-add y (mix64 (unchecked-add seed (unchecked-multiply salt 7919)))))))]
    (/ (double (unsigned-bit-shift-right k 11)) 9007199254740992.0)))

(defn- gauss ^double [^long x ^long y ^long seed ^long salt]
  ;; Irwin-Hall(3) shifted/scaled to ~unit variance.
  (* 2.0 (- (+ (uniform x y seed salt) (uniform x y seed (+ salt 100)) (uniform x y seed (+ salt 200))) 1.5)))

(defn grain!
  "Film grain. :amount 0..1, :size (1 = ~1px at 2000px long edge, scales with image),
  :chroma 0..1 (0 = monochrome grain), :seed int. Strongest in midtones."
  [buf {:keys [^double amount ^double size ^double chroma ^long seed]}]
  (let [{:keys [^long w ^long h ^floats rgb]} buf
        cell (Math/max 1 (Math/round (* size (/ (Math/max w h) 2000.0))))
        amp (* 0.25 amount)]
    (when (pos? amount)
      (buf/par-rows h
        (fn [^long y]
          (let [cy (quot y cell)]
            (dotimes [x w]
              (let [cx (quot x cell)
                    i (* 3 (+ (* y w) x))
                    l (luma rgb i)
                    wt (* amp (+ 0.35 (* 0.65 4.0 l (- 1.0 l))))
                    mono (* (- 1.0 chroma) (gauss cx cy seed 0))]
                (dotimes [c 3]
                  (let [j (+ i c)
                        n (if (pos? chroma) (+ mono (* chroma (gauss cx cy seed (inc c)))) mono)]
                    (aset rgb j (float (clamp01 (+ (aget rgb j) (* wt n)))))))))))))
    buf))

;; ---- light leak -----------------------------------------------------------

(def leak-positions
  {:top-left [0.0 0.0] :top [0.5 0.0] :top-right [1.0 0.0]
   :left [0.0 0.5] :right [1.0 0.5]
   :bottom-left [0.0 1.0] :bottom [0.5 1.0] :bottom-right [1.0 1.0]})

(defn light-leak!
  "Screens a warm glow radiating from an edge/corner. `size` is the glow radius as a fraction
  of the long edge; `color` is the hot core [r g b]; the rim shifts toward magenta/red."
  [buf position size [cr cg cb] seed]
  (let [{:keys [^long w ^long h ^floats rgb]} buf
        size (double size) cr (double cr) cg (double cg) cb (double cb)
        [px py] (leak-positions position)
        ox (* (double px) w) oy (* (double py) h)
        radius (* size (Math/max w h))
        phase (* 0.7 (double seed))]
    (buf/par-rows h
      (fn [^long y]
        (let [dy (- (+ y 0.5) oy)]
          (dotimes [x w]
            (let [dx (- (+ x 0.5) ox)
                  theta (Math/atan2 dy dx)
                  r (* radius (+ 1.0 (* 0.18 (Math/sin (+ (* 3.0 theta) phase)))))
                  f (clamp01 (- 1.0 (/ (Math/sqrt (+ (* dx dx) (* dy dy))) r)))
                  a (* f f)
                  ;; rim -> core colour gradient
                  lr (* a (+ cr (* f 0.0)))
                  lg (* a (+ (* cg 0.3) (* f 0.7 cg)))
                  lb (* a (+ (* cb 0.6) (* f 0.4 cb)))
                  i (* 3 (+ (* y w) x))]
              (aset rgb i (float (- 1.0 (* (- 1.0 (aget rgb i)) (- 1.0 lr)))))
              (aset rgb (+ i 1) (float (- 1.0 (* (- 1.0 (aget rgb (+ i 1))) (- 1.0 lg)))))
              (aset rgb (+ i 2) (float (- 1.0 (* (- 1.0 (aget rgb (+ i 2))) (- 1.0 lb))))))))))
    buf))
