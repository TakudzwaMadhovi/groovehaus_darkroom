(ns darkroom.imaging.camera
  "Camera colour profiles: turns a RAW file's white-balanced camera RGB into the
  editor's working colour space the way a DNG camera profile (.dcp, see
  darkroom.imaging.dcp) describes, following the DNG specification's chapter on
  mapping camera colours to XYZ:

    1. the as-shot white balance (the neutral the camera recorded) picks a point
       between the profile's two illuminants (by colour temperature, in mireds);
       the matrices are interpolated to it (an iteration, because the white's
       temperature depends on the matrix);
    2. camera RGB -> XYZ (D50) with the profile's ForwardMatrix, or, when it has
       none, the inverse of its ColorMatrix plus Bradford adaptation from the
       scene white to D50;
    3. the profile's hue / saturation / value table, then its look table, move
       colours in ProPhoto-primaries HSV (the rendering that gives a profile its
       style); optionally its tone curve;
    4. XYZ (D50) -> the working space.

  An ICC input profile (.icc / .icm, matrix or LUT based, as made by profiling
  software from a photographed colour chart) works too: the white-balanced camera
  RGB goes through the profile to the connection space (via a sampled 3D table
  of the Java colour engine's conversion) and on to the working space.

  Not implemented: the DCP's DefaultBlackRender and the embedded-profile policy
  flags (which only matter to software that writes profiles). Pure logic, no UI
  dependency."
  (:require [darkroom.imaging.color :as color]
            [darkroom.imaging.core :as core]
            [darkroom.imaging.dcp :as dcp]
            [darkroom.imaging.scene :as scene]
            [clojure.string :as str]))

(set! *unchecked-math* :warn-on-boxed)

;; ------------------------------------------------------------ illuminants

(def illuminant-cct
  "EXIF light-source code -> correlated colour temperature in kelvin (the values
  Adobe's DNG SDK uses)."
  {1 5500 2 4200 3 2850 4 5500 9 5500 10 6500 11 7500 12 6430 13 5000 14 4150 15 3500
   17 2856 18 4874 19 6774 20 5503 21 6504 22 7504 23 5003 24 3200})

(defn xyz->xy [[x y z]]
  (let [s (+ (double x) (double y) (double z))]
    [(/ (double x) s) (/ (double y) s)]))

(defn xy->cct
  "Correlated colour temperature of chromaticity [x y] (McCamy's approximation,
  good to a few kelvin between 2000 and 12500 K)."
  ^double [[x y]]
  (let [n (/ (- (double x) 0.3320) (- 0.1858 (double y)))]
    (+ (* 449.0 n n n) (* 3525.0 n n) (* 6823.3 n) 5520.33)))

(defn- lerp-vec [a b w]
  (let [w (double w)] (mapv (fn [x y] (+ (* w (double x)) (* (- 1.0 w) (double y)))) a b)))

(defn- ordered
  "The profile with its illuminant 1 the lower colour temperature (the DNG
  convention, but not every writer keeps to it): swaps the pairs when needed."
  [p]
  (let [t1 (illuminant-cct (:illuminant-1 p)) t2 (illuminant-cct (:illuminant-2 p))]
    (if (and t1 t2 (> (double t1) (double t2)))
      (assoc p :illuminant-1 (:illuminant-2 p) :illuminant-2 (:illuminant-1 p)
               :color-matrix-1 (:color-matrix-2 p) :color-matrix-2 (:color-matrix-1 p)
               :forward-matrix-1 (:forward-matrix-2 p) :forward-matrix-2 (:forward-matrix-1 p)
               :hue-sat-1 (:hue-sat-2 p) :hue-sat-2 (:hue-sat-1 p))
      p)))

(defn- weight
  "Share of illuminant 1 in the interpolation at colour temperature `t`
  (interpolated in 1/T, i.e. mireds; 1 at or below illuminant 1, 0 at or above 2)."
  ^double [^double t ^double t1 ^double t2]
  (cond (<= t t1) 1.0
        (>= t t2) 0.0
        :else (/ (- (/ 1.0 t) (/ 1.0 t2)) (- (/ 1.0 t1) (/ 1.0 t2)))))

(defn- dual? [p] (and (:color-matrix-2 p) (illuminant-cct (:illuminant-1 p)) (illuminant-cct (:illuminant-2 p))))

(defn interpolation-weight
  "Share of the profile's first illuminant at the scene white that `neutral`
  (the camera's response to white, [r g b]) implies. 1.0 for a single-illuminant profile."
  ^double [profile neutral]
  (let [p (ordered profile)]
    (if-not (dual? p)
      1.0
      (let [t1 (double (illuminant-cct (:illuminant-1 p))) t2 (double (illuminant-cct (:illuminant-2 p)))
            n (mapv double neutral)]
        (loop [xy color/d50-xy i 0]
          (let [w (weight (xy->cct xy) t1 t2)
                cm (lerp-vec (:color-matrix-1 p) (:color-matrix-2 p) w)
                xy' (xyz->xy (color/mat-vec (color/mat-inv cm) n))]
            (if (or (>= i 30) (and (< (Math/abs (- (double (xy' 0)) (double (xy 0)))) 1e-7)
                                   (< (Math/abs (- (double (xy' 1)) (double (xy 1)))) 1e-7)))
              w
              (recur xy' (inc i)))))))))

(defn camera->xyz
  "3x3 (row-major vector) taking white-balanced camera RGB (the recorded neutral
  scaled to equal channels) to XYZ with a D50 white, for `profile` and the
  camera's `neutral` (its raw response to the scene white, [r g b])."
  [profile neutral]
  (let [p (ordered profile)
        w (interpolation-weight p neutral)
        n (mapv double neutral)
        pick (fn [a b] (if (and a b (dual? p)) (lerp-vec a b w) (or a b)))]
    (if-let [fm (pick (:forward-matrix-1 p) (:forward-matrix-2 p))]
      (vec fm)
      (let [cm (pick (:color-matrix-1 p) (:color-matrix-2 p))
            inv (color/mat-inv cm)
            white (color/mat-vec inv n)
            k (/ 1.0 (double (white 1)))
            adapt (color/adaptation-matrix (xyz->xy white) color/d50-xy)]
        (color/mat* adapt (mapv #(* k (double %)) (color/mat* inv (color/mat-diag n))))))))

;; ------------------------------------------------------------ maps

(def ^:private romm-from-xyz-d50
  "XYZ (D50) -> linear ProPhoto (ROMM) RGB."
  [1.3459433 -0.2556075 -0.0511118 -0.5445989 1.5081673 0.0205351 0.0 0.0 1.2118128])

(defn- blend-tables
  "One table from the profile's two (or one) tables at weight `w`; nil when there is none."
  [t1 t2 w]
  (cond
    (and t1 t2 (not (identical? t1 t2)) (java.util.Arrays/equals ^longs (:dims t1) ^longs (:dims t2)))
    (let [^floats a (:data t1) ^floats b (:data t2) n (alength a) ^floats out (float-array n) w (double w)]
      (dotimes [i n] (aset out i (float (+ (* w (aget a i)) (* (- 1.0 w) (aget b i))))))
      (assoc t1 :data out))
    :else (or t1 t2)))

(defn- hsv-of
  "Writes [h s v] (h in 0-6) of rgb in [0,1] into `out`."
  [^double r ^double g ^double b ^doubles out]
  (let [mx (Math/max r (Math/max g b)) mn (Math/min r (Math/min g b)) d (- mx mn)]
    (aset out 2 mx)
    (if (<= mx 0.0)
      (do (aset out 0 0.0) (aset out 1 0.0))
      (let [s (/ d mx)
            h (double (cond (zero? d) 0.0
                            (== mx r) (let [x (/ (- g b) d)] (if (neg? x) (+ x 6.0) x))
                            (== mx g) (+ 2.0 (/ (- b r) d))
                            :else (+ 4.0 (/ (- r g) d))))]
        (aset out 0 (if (>= h 6.0) (- h 6.0) h)) (aset out 1 s)))))

(defn- rgb-of
  "Writes the rgb of (h in 0-6, s, v) into `out`."
  [^double h ^double s ^double v ^doubles out]
  (let [x (rem h 6.0)
        h (if (neg? x) (+ x 6.0) x)
        i (long (Math/floor h)) f (- h (double i))
        p (* v (- 1.0 s)) q (* v (- 1.0 (* s f))) t (* v (- 1.0 (* s (- 1.0 f))))]
    (case (int (min i 5))
      0 (do (aset out 0 v) (aset out 1 t) (aset out 2 p))
      1 (do (aset out 0 q) (aset out 1 v) (aset out 2 p))
      2 (do (aset out 0 p) (aset out 1 v) (aset out 2 t))
      3 (do (aset out 0 p) (aset out 1 q) (aset out 2 v))
      4 (do (aset out 0 t) (aset out 1 p) (aset out 2 v))
      (do (aset out 0 v) (aset out 1 p) (aset out 2 q)))))

(defn- table-delta!
  "Trilinear lookup in a hue/sat/val table (`dims` [hue sat val]) at `hsv`
  ([h in 0-6, s, v]): writes [hue-shift-degrees sat-scale val-scale] into `out`.
  Storage order is value (outer), hue, saturation (inner), as in the DNG SDK;
  hue wraps, the others clamp."
  [^floats data ^longs dims ^doubles hsv ^doubles out]
  (let [hd (aget dims 0) sd (aget dims 1) vd (aget dims 2)
        h (aget hsv 0) s (aget hsv 1) v (aget hsv 2)
        hf (* h (/ (double hd) 6.0))
        h-floor (long (Math/floor hf))
        hw (- hf (double h-floor))
        h0 (Math/floorMod h-floor hd)
        h1 (Math/floorMod (inc h-floor) hd)
        sf (* (Math/max 0.0 (Math/min 1.0 s)) (double (dec sd)))
        s0 (Math/max 0 (Math/min (long (Math/floor sf)) (- sd 2)))
        s1 (Math/min (dec sd) (inc s0))
        sw (if (== s0 s1) 0.0 (- sf (double s0)))
        vf (if (> vd 1) (* (Math/max 0.0 (Math/min 1.0 v)) (double (dec vd))) 0.0)
        v0 (if (> vd 1) (Math/max 0 (Math/min (long (Math/floor vf)) (- vd 2))) 0)
        v1 (Math/min (dec vd) (inc v0))
        vw (if (== v0 v1) 0.0 (- vf (double v0)))]
    (dotimes [c 3]
      (let [at (fn ^double [^long vi ^long hi ^long si] (double (aget data (+ (* 3 (+ (* (+ (* vi hd) hi) sd) si)) c))))
            lerp (fn ^double [^double a ^double b ^double t] (+ (* (- 1.0 t) a) (* t b)))
            plane (fn ^double [^long vi]
                    (lerp (lerp (at vi h0 s0) (at vi h0 s1) sw) (lerp (at vi h1 s0) (at vi h1 s1) sw) hw))]
        (aset out c (double (lerp (plane v0) (plane v1) vw)))))))

(defn- apply-table!
  "Moves the colour in `px` ([r g b], linear ProPhoto, each 0-1) through a
  hue/sat/val table with encoding flag `enc` (0 linear, 1 sRGB-encoded)."
  [{:keys [^longs dims data encoding]} ^doubles px ^doubles hsv ^doubles delta]
  (let [enc? (== 1 (long encoding))
        e (fn ^double [^double x] (if enc? (color/srgb-encode x) x))
        d (fn ^double [^double x] (if enc? (color/srgb-decode x) x))]
    (hsv-of (e (aget px 0)) (e (aget px 1)) (e (aget px 2)) hsv)
    (table-delta! data dims hsv delta)
    (let [h (+ (aget hsv 0) (/ (aget delta 0) 60.0))
          s (Math/max 0.0 (Math/min 1.0 (* (aget hsv 1) (aget delta 1))))
          v (Math/max 0.0 (Math/min 1.0 (* (aget hsv 2) (aget delta 2))))]
      (rgb-of h s v px)
      (aset px 0 (double (d (aget px 0)))) (aset px 1 (double (d (aget px 1)))) (aset px 2 (double (d (aget px 2)))))))

(defn- curve-lut
  "4097-entry table of the tone curve [[x y]..] over 0-1."
  ^floats [points]
  (let [pts (vec points) n (count pts) ^floats lut (float-array 4097)]
    (dotimes [i 4097]
      (let [x (/ (double i) 4096.0)
            j (first (keep-indexed (fn [k [px _]] (when (>= (double px) x) k)) pts))]
        (aset lut i (float (cond (nil? j) (double (second (peek pts)))
                                 (zero? (long j)) (double (second (first pts)))
                                 :else (let [[x0 y0] (pts (dec (long j))) [x1 y1] (pts j)
                                             span (- (double x1) (double x0))]
                                         (if (zero? span) (double y1)
                                             (+ (double y0) (* (- (double y1) (double y0)) (/ (- x (double x0)) span))))))))))
    lut))

(defn- curve-at ^double [^floats lut ^double x]
  (let [f (* 4096.0 (Math/max 0.0 (Math/min 1.0 x)))
        i (long f) j (Math/min 4096 (inc i)) t (- f (double i))]
    (+ (* (- 1.0 t) (aget lut i)) (* t (aget lut j)))))

;; ------------------------------------------------------------ rendering

(declare render-dcp render-icc)

(defn render
  "The working-space scene image for `cam`, a float image {:width :height :data}
  of white-balanced linear camera RGB (65535 -> 1.0), through `profile` (parsed
  .dcp) for a camera whose recorded white is `neutral` ([r g b]).
  opts :tone-curve? applies the profile's own tone curve."
  [{:keys [^long width ^long height data] :as cam} profile neutral & [{:keys [tone-curve?]}]]
  (if (:icc profile)
    (render-icc cam profile)
    (render-dcp cam profile neutral tone-curve?)))

(defn- render-dcp
  [{:keys [^long width ^long height data] :as cam} profile neutral tone-curve?]
  (let [p (ordered profile)
        w (interpolation-weight p neutral)
        m (camera->xyz p neutral)
        gain (Math/pow 2.0 (double (:baseline-exposure p)))
        hue-sat (blend-tables (:hue-sat-1 p) (:hue-sat-2 p) w)
        look (:look p)
        curve (when (and tone-curve? (:tone-curve p)) (curve-lut (:tone-curve p)))
        to-working (color/mat* (color/xyz->rgb-matrix :working) (color/adaptation-matrix color/d50-xy color/d65-xy))
        maps? (or hue-sat look curve)
        ;; with no maps the whole chain is one matrix
        direct ^doubles (double-array (color/mat* to-working (mapv #(* gain (double %)) m)))
        ^doubles cm (double-array (mapv #(* gain (double %)) m))
        ^doubles romm (double-array romm-from-xyz-d50)
        ^doubles romm-inv (double-array (color/mat-inv romm-from-xyz-d50))
        ^doubles tw (double-array to-working)
        ^floats src data
        n (* width height)
        ^floats out (float-array (* 3 n))]
    (core/parallel-ranges!
      n
      (fn [^long start ^long end]
        (let [px (double-array 3) hsv (double-array 3) delta (double-array 3)]
          (loop [i start]
            (when (< i end)
              (let [j (* 3 i) r (double (aget src j)) g (double (aget src (+ j 1))) b (double (aget src (+ j 2)))]
                (if-not maps?
                  (do (aset out j (float (+ (* (aget direct 0) r) (* (aget direct 1) g) (* (aget direct 2) b))))
                      (aset out (+ j 1) (float (+ (* (aget direct 3) r) (* (aget direct 4) g) (* (aget direct 5) b))))
                      (aset out (+ j 2) (float (+ (* (aget direct 6) r) (* (aget direct 7) g) (* (aget direct 8) b)))))
                  (let [x (+ (* (aget cm 0) r) (* (aget cm 1) g) (* (aget cm 2) b))
                        y (+ (* (aget cm 3) r) (* (aget cm 4) g) (* (aget cm 5) b))
                        z (+ (* (aget cm 6) r) (* (aget cm 7) g) (* (aget cm 8) b))
                        pr (Math/max 0.0 (+ (* (aget romm 0) x) (* (aget romm 1) y) (* (aget romm 2) z)))
                        pg (Math/max 0.0 (+ (* (aget romm 3) x) (* (aget romm 4) y) (* (aget romm 5) z)))
                        pb (Math/max 0.0 (+ (* (aget romm 6) x) (* (aget romm 7) y) (* (aget romm 8) z)))
                        mx (Math/max 1.0 (Math/max pr (Math/max pg pb)))] ; over-range colours are mapped as if clipped, then scaled back
                    (aset px 0 (/ pr mx)) (aset px 1 (/ pg mx)) (aset px 2 (/ pb mx))
                    (when hue-sat (apply-table! hue-sat px hsv delta))
                    (when look (apply-table! look px hsv delta))
                    (let [cr (* mx (aget px 0)) cg (* mx (aget px 1)) cb (* mx (aget px 2))
                          cr (if curve (if (<= cr 1.0) (curve-at curve cr) cr) cr)
                          cg (if curve (if (<= cg 1.0) (curve-at curve cg) cg) cg)
                          cb (if curve (if (<= cb 1.0) (curve-at curve cb) cb) cb)
                          x2 (+ (* (aget romm-inv 0) cr) (* (aget romm-inv 1) cg) (* (aget romm-inv 2) cb))
                          y2 (+ (* (aget romm-inv 3) cr) (* (aget romm-inv 4) cg) (* (aget romm-inv 5) cb))
                          z2 (+ (* (aget romm-inv 6) cr) (* (aget romm-inv 7) cg) (* (aget romm-inv 8) cb))]
                      (aset out j (float (+ (* (aget tw 0) x2) (* (aget tw 1) y2) (* (aget tw 2) z2))))
                      (aset out (+ j 1) (float (+ (* (aget tw 3) x2) (* (aget tw 4) y2) (* (aget tw 5) z2))))
                      (aset out (+ j 2) (float (+ (* (aget tw 6) x2) (* (aget tw 7) y2) (* (aget tw 8) z2))))))))
              (recur (inc i)))))))
    (scene/image width height out)))

;; ------------------------------------------------------------ ICC input profiles

(def ^:private ^:const lut-n 33)
(def ^:private ^:const lut-gamma 2.4)

(defn icc-profile
  "{:icc bytes :name description} for the bytes of an RGB ICC profile; throws
  ex-info when it is not a usable three-channel RGB profile."
  [^bytes bs]
  (let [^java.awt.color.ICC_Profile p (try (java.awt.color.ICC_Profile/getInstance bs)
                                           (catch Throwable t (throw (ex-info "Not an ICC profile" {} t))))]
    (when-not (and (== 3 (.getNumComponents p)) (== java.awt.color.ColorSpace/TYPE_RGB (.getColorSpaceType p)))
      (throw (ex-info "The ICC profile is not an RGB profile" {})))
    (let [^bytes d (.getData p java.awt.color.ICC_Profile/icSigProfileDescriptionTag)
          ;; 'desc' (v2): type, reserved, count, ASCII; 'mluc' (v4): UTF-16BE record
          desc (try (cond (nil? d) nil
                          (= "desc" (String. d 0 4 "US-ASCII"))
                          (let [n (.getInt (java.nio.ByteBuffer/wrap d) 8)] (String. d 12 (Math/max 0 (dec n)) "US-ASCII"))
                          (= "mluc" (String. d 0 4 "US-ASCII"))
                          (let [bb (java.nio.ByteBuffer/wrap d) len (.getInt bb 20) off (.getInt bb 24)] (String. d off len "UTF-16BE"))
                          :else nil)
                    (catch Throwable _ nil))]
      {:icc bs :name (or (not-empty (str/trim (str desc))) "ICC profile")})))

(defn- icc-lut
  "Float array [n n n 3] of XYZ (D50) for camera RGB at nodes spaced in x^(1/2.4)
  (dense near black), computed by the JDK's colour engine from the profile."
  ^floats [{:keys [^bytes icc]}]
  (let [cs (java.awt.color.ICC_ColorSpace. (java.awt.color.ICC_Profile/getInstance icc))
        n lut-n ^floats lut (float-array (* n n n 3))
        node (fn ^double [^long i] (Math/pow (/ (double i) (double (dec n))) lut-gamma))]
    (dotimes [ri n]
      (dotimes [gi n]
        (dotimes [bi n]
          (let [xyz (.toCIEXYZ cs (float-array [(node ri) (node gi) (node bi)]))
                o (* 3 (+ (* (+ (* ri n) gi) n) bi))]
            (aset lut o (aget xyz 0)) (aset lut (+ o 1) (aget xyz 1)) (aset lut (+ o 2) (aget xyz 2))))))
    lut))

(defonce ^:private icc-luts (atom {}))

(defn- lut-for ^floats [{:keys [^bytes icc] :as profile}]
  (let [k (java.util.Arrays/hashCode icc)]
    (or (@icc-luts k) (let [l (icc-lut profile)] (reset! icc-luts {k l}) l))))

(defn render-icc
  "Working-space scene image for `cam` (white-balanced linear camera RGB, 65535
  -> 1.0) through the ICC input profile `profile` (see icc-profile). Values above
  1.0 keep their colour and scale: the colour of the clipped value is looked up
  and multiplied back."
  [{:keys [^long width ^long height data]} profile]
  (let [^floats lut (lut-for profile)
        to-working (color/mat* (color/xyz->rgb-matrix :working) (color/adaptation-matrix color/d50-xy color/d65-xy))
        ^doubles tw (double-array to-working)
        ^floats src data
        n (* width height) ^floats out (float-array (* 3 n))
        inv-g (/ 1.0 lut-gamma) top (double (dec lut-n))]
    (core/parallel-ranges!
      n
      (fn [^long start ^long end]
        (let [xyz (double-array 3)]
          (loop [i start]
            (when (< i end)
              (let [j (* 3 i)
                    r (Math/max 0.0 (double (aget src j))) g (Math/max 0.0 (double (aget src (+ j 1)))) b (Math/max 0.0 (double (aget src (+ j 2))))
                    m (Math/max 1.0 (Math/max r (Math/max g b)))
                    ;; position in node units
                    fr (* top (Math/pow (/ r m) inv-g)) fg (* top (Math/pow (/ g m) inv-g)) fb (* top (Math/pow (/ b m) inv-g))
                    r0 (Math/min (- lut-n 2) (long fr)) g0 (Math/min (- lut-n 2) (long fg)) b0 (Math/min (- lut-n 2) (long fb))
                    tr (- fr r0) tg (- fg g0) tb (- fb b0)]
                (dotimes [c 3]
                  (let [at (fn ^double [^long ri ^long gi ^long bi] (double (aget lut (+ (* 3 (+ (* (+ (* ri lut-n) gi) lut-n) bi)) c))))
                        l (fn ^double [^double a ^double bb ^double t] (+ (* (- 1.0 t) a) (* t bb)))
                        c00 (l (at r0 g0 b0) (at (inc r0) g0 b0) tr) c10 (l (at r0 (inc g0) b0) (at (inc r0) (inc g0) b0) tr)
                        c01 (l (at r0 g0 (inc b0)) (at (inc r0) g0 (inc b0)) tr) c11 (l (at r0 (inc g0) (inc b0)) (at (inc r0) (inc g0) (inc b0)) tr)]
                    (aset xyz c (* m (double (l (l c00 c10 tg) (l c01 c11 tg) tb))))))
                (let [x (aget xyz 0) y (aget xyz 1) z (aget xyz 2)]
                  (aset out j (float (+ (* (aget tw 0) x) (* (aget tw 1) y) (* (aget tw 2) z))))
                  (aset out (+ j 1) (float (+ (* (aget tw 3) x) (* (aget tw 4) y) (* (aget tw 5) z))))
                  (aset out (+ j 2) (float (+ (* (aget tw 6) x) (* (aget tw 7) y) (* (aget tw 8) z))))))
              (recur (inc i)))))))
    (scene/image width height out)))

(defonce ^:private profile-cache (atom {}))

(defn load-profile
  "Parsed camera profile for `path` (a .dcp, or an .icc / .icm input profile),
  remembered until the file changes."
  [path]
  (let [f (java.io.File. (str path)) k [(.getPath f) (.lastModified f)]]
    (or (get @profile-cache k)
        (let [p (if (re-find #"(?i)\.ic[cm]$" (.getName f))
                  (icc-profile (java.nio.file.Files/readAllBytes (.toPath f)))
                  (dcp/read-file f))]
          (swap! profile-cache (fn [m] (assoc (into {} (remove (fn [[[pp _] _]] (= pp (.getPath f))) m)) k p)))
          p))))
