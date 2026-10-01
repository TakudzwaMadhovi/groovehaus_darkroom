(ns darkroom.imaging.color
  "Colour science: RGB primaries, 3x3 matrices, transfer curves, and the
  conversions between the editing (working) space and output spaces. Pure
  logic, no UI dependency.

  Matrices are vectors of 9 doubles in row-major order. Every RGB space here
  has a D65 white point, so converting between two of them is a single matrix
  (no chromatic adaptation); ICC profiles need D50 colorants, which `icc`
  handles with a Bradford adaptation.")


;; ------------------------------------------------------------- 3x3 algebra

(defn mat*
  "Matrix product a·b."
  [a b]
  (vec (for [i (range 3) j (range 3)]
         (reduce + (for [k (range 3)] (* (double (a (+ (* 3 i) k))) (double (b (+ (* 3 k) j)))))))))

(defn mat-vec
  "Matrix times column vector [x y z]."
  [m [x y z]]
  (let [x (double x) y (double y) z (double z)]
    (vec (for [i (range 3)]
           (+ (* (double (m (* 3 i))) x) (* (double (m (+ (* 3 i) 1))) y) (* (double (m (+ (* 3 i) 2))) z))))))

(defn mat-inv
  "Inverse of a 3x3 matrix. Throws if it is singular."
  [m]
  (let [[a b c d e f g h i] (map double m)
        co00 (- (* e i) (* f h)) co01 (- (* f g) (* d i)) co02 (- (* d h) (* e g))
        det  (+ (* a co00) (* b co01) (* c co02))]
    (when (< (Math/abs (double det)) 1e-12) (throw (ex-info "Singular matrix" {:matrix m})))
    (let [k (/ 1.0 det)]
      [(* k co00) (* k (- (* c h) (* b i))) (* k (- (* b f) (* c e)))
       (* k co01) (* k (- (* a i) (* c g))) (* k (- (* c d) (* a f)))
       (* k co02) (* k (- (* b g) (* a h))) (* k (- (* a e) (* b d)))])))

(defn mat-diag [[x y z]] [x 0.0 0.0 0.0 y 0.0 0.0 0.0 z])

;; ------------------------------------------------------------ white points

(def d65-xy [0.3127 0.3290])
(def d50-xy [0.3457 0.3585])

(defn xy->xyz
  "Chromaticity to XYZ with Y = 1."
  [[x y]]
  (let [x (double x) y (double y)]
    [(/ x y) 1.0 (/ (- 1.0 x y) y)]))

;; ------------------------------------------------------------------ spaces

(def spaces
  "RGB spaces: primaries (xy) or a matrix :from-srgb, white point, transfer
  curve. :trc is :srgb or [:gamma g]."
  {:srgb       {:label "sRGB"
                :primaries [[0.64 0.33] [0.30 0.60] [0.15 0.06]] :white d65-xy :trc :srgb}
   :display-p3 {:label "Display P3"
                :primaries [[0.68 0.32] [0.265 0.69] [0.15 0.06]] :white d65-xy :trc :srgb}
   :adobe-rgb  {:label "Adobe RGB (1998)"
                :primaries [[0.64 0.33] [0.21 0.71] [0.15 0.06]] :white d65-xy
                :trc [:gamma 2.19921875]}
   ;; The editing space: what LibRaw calls "ProPhoto D65" (output colour 4).
   ;; It is dcraw's table, not true ProPhoto (measured primaries R 0.735,0.261
   ;; G 0.138,0.878 B 0.069,0.014), so it is defined by its matrix from linear
   ;; sRGB. raw-test re-measures it from a synthetic DNG to guard against drift.
   :working    {:label "Working (wide gamut)"
                :from-srgb [0.529317 0.330092 0.140588
                            0.098368 0.873465 0.028169
                            0.016879 0.117663 0.865457]
                :white d65-xy :trc :srgb}})

(defn rgb->xyz-matrix
  "Matrix taking linear RGB of `space` to XYZ (white = Y 1)."
  [space]
  (let [{:keys [primaries white from-srgb]} (spaces space)]
    (if from-srgb
      (mat* (rgb->xyz-matrix :srgb) (mat-inv from-srgb))
      (let [cols (mapv xy->xyz primaries)
            p    (vec (for [i (range 3) j (range 3)] ((cols j) i))) ; columns = primaries
            s    (mat-vec (mat-inv p) (xy->xyz white))]
        (mat* p (mat-diag s))))))

(defn xyz->rgb-matrix [space] (mat-inv (rgb->xyz-matrix space)))

(defn convert-matrix
  "Matrix taking linear RGB of space `from` to linear RGB of space `to`."
  [from to]
  (mat* (xyz->rgb-matrix to) (rgb->xyz-matrix from)))

(defn luma-weights
  "[wr wg wb]: relative luminance of linear RGB in `space`."
  [space]
  (let [m (rgb->xyz-matrix space)]
    [(m 3) (m 4) (m 5)]))

;; ---------------------------------------------------------- transfer curves

(defn srgb-encode
  "Linear -> sRGB-encoded. Defined for x >= 0 (not clamped above 1)."
  ^double [^double x]
  (if (<= x 0.0031308) (* 12.92 x) (- (* 1.055 (Math/pow x (/ 1.0 2.4))) 0.055)))

(defn srgb-decode
  "sRGB-encoded -> linear."
  ^double [^double v]
  (if (<= v 0.04045) (/ v 12.92) (Math/pow (/ (+ v 0.055) 1.055) 2.4)))

(defn trc-encode-fn
  "(fn ^double [^double linear]) for a :trc spec."
  [trc]
  (if (= trc :srgb)
    srgb-encode
    (let [inv (/ 1.0 (double (second trc)))] (fn ^double [^double x] (Math/pow x inv)))))

(defn trc-decode-fn
  [trc]
  (if (= trc :srgb)
    srgb-decode
    (let [g (double (second trc))] (fn ^double [^double v] (Math/pow v g)))))

;; ------------------------------------------------------- Bradford adaptation

(def ^:private bradford [0.8951 0.2664 -0.1614 -0.7502 1.7135 0.0367 0.0389 -0.0685 1.0296])

(defn adaptation-matrix
  "XYZ -> XYZ Bradford chromatic adaptation from white `src-xy` to `dst-xy`."
  [src-xy dst-xy]
  (let [s (mat-vec bradford (xy->xyz src-xy))
        d (mat-vec bradford (xy->xyz dst-xy))]
    (mat* (mat-inv bradford) (mat* (mat-diag (mapv #(/ (double %1) (double %2)) d s)) bradford))))

;; ------------------------------------------------ white balance (temp / tint)

(defn- planck-uv
  "CIE 1960 (u, v) of a Planckian radiator at `t` kelvin (Krystek 1985,
  valid 1000-15000 K)."
  [t]
  (let [t (double t) t2 (* t t)]
    [(/ (+ 0.860117757 (* 1.54118254e-4 t) (* 1.28641212e-7 t2))
        (+ 1.0 (* 8.42420235e-4 t) (* 7.08145163e-7 t2)))
     (/ (+ 0.317398726 (* 4.22806245e-5 t) (* 4.20481691e-8 t2))
        (- (+ 1.0 (* 1.61456053e-7 t2)) (* 2.89741816e-5 t)))]))

(defn- xy->uv [[x y]]
  (let [x (double x) y (double y) d (+ (* -2.0 x) (* 12.0 y) 3.0)]
    [(/ (* 4.0 x) d) (/ (* 6.0 y) d)]))

(defn- uv->xy [[u v]]
  (let [u (double u) v (double v) d (- (* 2.0 u) (* 8.0 v) -4.0)]
    [(/ (* 3.0 u) d) (/ (* 2.0 v) d)]))

(def ^:private warm-mireds
  "Slider range of the temperature control, in mired either side of D65."
  80.0)

(def ^:private tint-uv 0.02)

(defn wb-white
  "The white point the photo is *assumed to have been shot under* for the
  temperature/tint sliders (each -1..1; 0, 0 is D65). Warming (+temp) assumes a
  bluer source; adapting that source to D65 adds yellow. Tint moves the source
  perpendicular to the Planckian locus (+tint = more magenta in the result)."
  [temp tint]
  (let [temp (double temp) tint (double tint)
        t65  (/ 1.0e6 153.8)
        [u0 v0] (xy->uv d65-xy)
        [up vp] (planck-uv t65)
        mired (- 153.8 (* temp warm-mireds))
        [ut vt] (planck-uv (/ 1.0e6 mired))]
    (uv->xy [(+ u0 (- (double ut) (double up)))
             (+ v0 (- (double vt) (double vp)) (* tint tint-uv))])))

(defn wb-matrix
  "Linear working-RGB -> working-RGB matrix for the temperature/tint sliders
  (identity at 0, 0)."
  [temp tint]
  (let [to-xyz (rgb->xyz-matrix :working)
        adapt  (adaptation-matrix (wb-white temp tint) d65-xy)]
    (mat* (mat-inv to-xyz) (mat* adapt to-xyz))))

;; ------------------------------------------------------------- ICC profiles

(declare icc-profile-bytes)

(defn- s15f16 ^long [x] (Math/round (* (double x) 65536.0)))

(defn- be-bytes
  "Big-endian bytes of the 32-bit ints in `xs`."
  [xs]
  (let [bb (java.nio.ByteBuffer/allocate (* 4 (count xs)))]
    (doseq [x xs] (.putInt bb (unchecked-int (long x))))
    (.array bb)))

(defn- ascii4 [^String s] (.getBytes s "US-ASCII"))

(defn- pad4 ^bytes [^bytes bs]
  (let [n (alength bs) m (mod (- 4 (mod n 4)) 4)]
    (if (zero? m) bs (java.util.Arrays/copyOf bs (int (+ n m))))))

(defn- cat-bytes ^bytes [& parts]
  (let [out (java.io.ByteArrayOutputStream.)]
    (doseq [^bytes p parts] (.write out p 0 (alength p)))
    (.toByteArray out)))

(defn- xyz-tag [[x y z]]
  (cat-bytes (ascii4 "XYZ ") (be-bytes [0 (s15f16 x) (s15f16 y) (s15f16 z)])))

(defn- text-tag [^String s]
  (cat-bytes (ascii4 "text") (be-bytes [0]) (.getBytes s "US-ASCII") (byte-array 1)))

(defn- desc-tag [^String s]
  (let [a (.getBytes s "US-ASCII")]
    (cat-bytes (ascii4 "desc") (be-bytes [0 (inc (alength a))]) a (byte-array 1)
               (be-bytes [0 0]) (byte-array 2) (byte-array 1) (byte-array 67))))

(defn- curve-tag
  "'curv' tag: a 1024-point table of the decode curve (device -> linear)."
  [trc]
  (let [decode (trc-decode-fn trc)
        n 1024
        bb (java.nio.ByteBuffer/allocate (+ 12 (* 2 n)))]
    (.put bb ^bytes (ascii4 "curv")) (.putInt bb 0) (.putInt bb (int n))
    (dotimes [i n]
      (.putShort bb (unchecked-short (Math/round (* 65535.0 (double (decode (/ (double i) (double (dec n))))))))))
    (.array bb)))

(defn icc-bytes
  "ICC profile bytes to embed for output `space`: the JDK's standard sRGB
  profile for :srgb (most widely recognised), a generated matrix/TRC profile for
  the others."
  ^bytes [space]
  (if (= space :srgb)
    (.getData (java.awt.color.ICC_Profile/getInstance java.awt.color.ColorSpace/CS_sRGB))
    (icc-profile-bytes space)))

(defn icc-profile-bytes
  "A minimal ICC v2 matrix/TRC display profile for `space` (D50 PCS colorants
  via Bradford adaptation). Embed it in exported files so other software
  interprets the pixels correctly."
  ^bytes [space]
  (let [{:keys [label trc]} (spaces space)
        m   (mat* (adaptation-matrix d65-xy d50-xy) (rgb->xyz-matrix space))
        d50 (xy->xyz d50-xy)
        col (fn [j] [(m j) (m (+ j 3)) (m (+ j 6))])
        trc-tag (curve-tag trc)
        tags [["desc" (desc-tag label)]
              ["cprt" (text-tag "Generated by Groovehaus Darkroom; no copyright")]
              ["wtpt" (xyz-tag d50)]
              ["rXYZ" (xyz-tag (col 0))] ["gXYZ" (xyz-tag (col 1))] ["bXYZ" (xyz-tag (col 2))]
              ["rTRC" trc-tag] ["gTRC" trc-tag] ["bTRC" trc-tag]]
        table-len (+ 4 (* 12 (count tags)))
        bodies (mapv (comp pad4 second) tags)
        offsets (reductions + (+ 128 table-len) (map #(alength ^bytes %) bodies))
        total   (last offsets)
        header  (let [bb (java.nio.ByteBuffer/allocate 128)]
                  (.putInt bb (int total))
                  (.putInt bb 0)
                  (.putInt bb 0x02400000)               ; v2.4
                  (.put bb ^bytes (ascii4 "mntr")) (.put bb ^bytes (ascii4 "RGB ")) (.put bb ^bytes (ascii4 "XYZ "))
                  (doseq [v [2024 1 1 0 0 0]] (.putShort bb (short v)))
                  (.put bb ^bytes (ascii4 "acsp"))
                  (.position bb 64)                      ; intent 0 (perceptual)
                  (.putInt bb 0)
                  (.putInt bb (int (s15f16 (d50 0)))) (.putInt bb (int (s15f16 (d50 1)))) (.putInt bb (int (s15f16 (d50 2))))
                  (.array bb))
        table (be-bytes (cons (count tags)
                              (mapcat (fn [[sig _] off ^bytes body]
                                        [(.getInt (java.nio.ByteBuffer/wrap (ascii4 sig))) off
                                         (alength ^bytes (second (first (filter #(= sig (first %)) tags))))])
                                      tags offsets bodies)))]
    (apply cat-bytes header table bodies)))
