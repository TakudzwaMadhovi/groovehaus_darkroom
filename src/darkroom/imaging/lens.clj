(ns darkroom.imaging.lens
  "Lens profiles from a lensfun database (the open lens-calibration collection
  used by darktable and RawTherapee; point the app at its `data/db` folder of
  XML files). Finds the profile for a picture's lens and focal length and turns
  it into the :lens-profile setting that darkroom.imaging.geometry applies:
  distortion (poly3, poly5 or ptlens model) and linear lateral chromatic
  aberration. Vignetting calibrations and the ACM / poly3 TCA models are not
  used, and the lens-centre offset some entries carry is ignored.

  The maths follows lensfun's own source (libs/lensfun: lens.cpp for choosing
  and interpolating calibrations, modifier.cpp and mod-coord.cpp for the
  coordinate system and `rescale_polynomial_coefficients`, mod-subpix.cpp for
  TCA):
  - Calibrations are in Hugin's coordinates (r = 1 is half the image height of
    the calibration camera). They are rescaled into the unit the correction runs
    in: the picture's coordinates in focal lengths on the sensor.
  - A point at undistorted radius Ru is found in the picture at Ru * k(Ru), with
      poly3   k = 1 + k1 Ru^2        poly5   k = 1 + k1 Ru^2 + k2 Ru^4
      ptlens  k = 1 + a Ru^3 + b Ru^2 + c Ru
    (k1, k2, a, b, c rescaled as described).
  - Between calibrated focal lengths lensfun interpolates a cubic Hermite spline
    through the neighbouring calibrations, on the terms multiplied by their focal
    length (divided again afterwards), and on the real focal length.
  - Red and blue are sampled at the distorted position scaled by kr and kb.
  Pure logic, no UI dependency."
  (:require [clojure.set :as set]
            [clojure.string :as str])
  (:import (java.io ByteArrayInputStream File)
           (javax.xml.parsers DocumentBuilderFactory)
           (org.w3c.dom Document Element NodeList)))

(def ^:private sensor-diagonal 43.2666153056) ; mm of a 36 x 24 frame

;; ----------------------------------------------------------------- parsing

(defn- parse-doc ^Document [^bytes bs]
  ;; lensfun files declare a DOCTYPE that points at a DTD: allow the declaration
  ;; but never fetch or expand anything external
  (let [f (doto (DocumentBuilderFactory/newInstance)
            (.setFeature "http://apache.org/xml/features/nonvalidating/load-external-dtd" false)
            (.setFeature "http://xml.org/sax/features/external-general-entities" false)
            (.setFeature "http://xml.org/sax/features/external-parameter-entities" false)
            (.setFeature javax.xml.XMLConstants/FEATURE_SECURE_PROCESSING true)
            (.setXIncludeAware false)
            (.setExpandEntityReferences false))]
    (.parse (.newDocumentBuilder f) (ByteArrayInputStream. bs))))

(defn- elements [^Element e tag]
  (let [^NodeList nl (.getElementsByTagName e tag)]
    (filterv #(= e (.getParentNode ^org.w3c.dom.Node %)) (for [i (range (.getLength nl))] (.item nl i)))))

(defn- text-of [^Element e tag] (some-> (first (elements e tag)) .getTextContent str/trim not-empty))

(defn- parse-num [s] (when-not (str/blank? s) (try (Double/parseDouble (str/trim s)) (catch NumberFormatException _ nil))))

(defn- num-attr [^Element e a] (parse-num (.getAttribute e a)))

(defn- parse-aspect
  "\"3:2\" or \"1.5\" -> 1.5."
  [s]
  (when-not (str/blank? s)
    (if-let [[_ a b] (re-find #"^\s*([\d.]+)\s*:\s*([\d.]+)" s)]
      (/ (parse-num a) (parse-num b))
      (parse-num s))))

(def ^:private term-attrs
  {:poly3 [["k1"]] :poly5 [["k1"] ["k2"]] :ptlens [["a"] ["b"] ["c"]]})

(defn- distortion-entry
  "{:focal :model :terms :real-focal}, with lensfun's default for a missing
  real-focal (database.cpp): ptlens focal*(1-a-b-c), poly3 focal*(1-k1), else focal."
  [^Element e]
  (let [model (keyword (.getAttribute e "model"))
        names (term-attrs model)
        terms (mapv (fn [[n]] (num-attr e n)) names)
        focal (num-attr e "focal")]
    (when (and focal (seq terms) (every? some? terms))
      (let [real (num-attr e "real-focal")]
        {:focal focal :model model :terms terms
         :real-focal (if (and real (pos? real))
                       real
                       (case model
                         :ptlens (* focal (- 1.0 (reduce + terms)))
                         :poly3 (* focal (- 1.0 (first terms)))
                         focal))}))))

(defn- tca-entry [^Element e]
  (let [focal (num-attr e "focal")
        kr (or (num-attr e "kr") 1.0) kb (or (num-attr e "kb") 1.0)]
    (when (and focal (= "linear" (.getAttribute e "model")))
      {:focal focal :kr kr :kb kb})))

(defn parse-database
  "Lenses from the bytes of one lensfun XML file:
  [{:maker :model :calibrations [{:crop :aspect :distortion [..] :tca [..]}]}].
  Lenses without any distortion calibration are left out. A <calibration> may
  carry its own cropfactor / aspect-ratio; otherwise the lens's are used."
  [^bytes xml]
  (let [root (.getDocumentElement (parse-doc xml))]
    (vec (for [^Element l (elements root "lens")
               :let [maker (text-of l "maker")
                     model (text-of l "model")
                     lens-crop (or (parse-num (text-of l "cropfactor")) 1.0)
                     lens-aspect (or (parse-aspect (text-of l "aspect-ratio")) 1.5)
                     sets (vec (for [^Element c (elements l "calibration")
                                     :let [crop (or (num-attr c "cropfactor") lens-crop)
                                           aspect (or (parse-aspect (.getAttribute c "aspect-ratio")) lens-aspect)
                                           dist (vec (sort-by :focal (keep distortion-entry (elements c "distortion"))))
                                           tca (vec (sort-by :focal (keep tca-entry (elements c "tca"))))]
                                     :when (or (seq dist) (seq tca))]
                                 {:crop crop :aspect aspect :distortion dist :tca tca}))]
               :when (and model (some (comp seq :distortion) sets))]
           {:maker maker :model model :calibrations sets}))))

(defn load-database
  "All lenses of the .xml files directly inside `dir` (unreadable files are skipped)."
  [^File dir]
  (vec (mapcat (fn [^File f]
                 (try (parse-database (java.nio.file.Files/readAllBytes (.toPath f)))
                      (catch Throwable _ [])))
               (sort-by #(.getName ^File %) (filter #(.endsWith (.toLowerCase (.getName ^File %)) ".xml") (or (.listFiles dir) []))))))

;; ----------------------------------------------------------------- matching

(def ^:private noise-words
  "Words makers and cameras add or drop at will (\"Zoom-Nikkor\" / \"Nikkor\")."
  #{"zoom" "lens"})

(defn- tokens [s]
  (set (remove #(or (str/blank? %) (noise-words %))
               (str/split (str/lower-case (str/replace (str s) #"[^A-Za-z0-9.]+" " ")) #"\s+"))))

(defn find-lenses
  "The database lenses that match the picture's lens, best match first (several
  entries often share a name, one per calibrated sensor size). `tags` are the
  camera tags (exif/read-tags): :lens-model, and :lens-make / :make for the
  maker. A lens matches when every word of its name (without its maker's name,
  which cameras write in many ways) appears in the camera's lens string; the
  longest names win, so \"AF-S Nikkor 50mm f/1.8G\" never takes the profile of
  the older \"AF Nikkor 50mm f/1.8D\" or of \"... f/1.8G 176\"."
  [db {:keys [lens-model]}]
  (when-not (str/blank? lens-model)
    (let [have (tokens lens-model)
          scored (for [l db
                       :let [want (set/difference (tokens (:model l)) (tokens (:maker l)))]
                       :when (and (seq want) (every? have want))]
                   [(count want) l])
          best (reduce max 0 (map first scored))]
      (vec (for [[n l] scored :when (= n best)] l)))))

(defn find-lens
  "The first of `find-lenses`, or nil."
  [db tags]
  (first (find-lenses db tags)))

;; ------------------------------------------------------------- profile maths

(defn- hermite
  "lensfun's _lf_interpolate: cubic Hermite between y2 and y3 at t (0-1), with
  neighbours y1 and y4 for the tangents (nil when absent)."
  [y1 y2 y3 y4 t]
  (let [t2 (* t t) t3 (* t2 t)
        tg2 (if y1 (* 0.5 (- y3 y1)) (- y3 y2))
        tg3 (if y4 (* 0.5 (- y4 y2)) (- y3 y2))]
    (+ (* (+ (- (* 2 t3) (* 3 t2)) 1) y2)
       (* (+ (- t3 (* 2 t2)) t) tg2)
       (* (- (* 3 t2) (* 2 t3)) y3)
       (* (- t3 t2) tg3))))

(defn- neighbours
  "[below2 below1 above1 above2] of `entries` around `focal` (nil where there is
  none), as lensfun's spline picks them."
  [entries focal]
  (let [below (reverse (filter #(< (:focal %) focal) (sort-by :focal entries)))
        above (filter #(> (:focal %) focal) (sort-by :focal entries))]
    [(second below) (first below) (first above) (second above)]))

(defn- interpolate
  "Entry values at `focal`: `value-fns` map an entry to a number (or nil), `scaled?`
  says whether to interpolate value*focal (lensfun does that for distortion terms)."
  [entries focal value-fns scaled?]
  (if-let [exact (first (filter #(== focal (:focal %)) entries))]
    (mapv #(% exact) value-fns)
    (let [[b2 b1 a1 a2] (neighbours entries focal)]
      (cond
        (and b1 a1)
        (let [t (/ (- focal (:focal b1)) (- (:focal a1) (:focal b1)))]
          (mapv (fn [f]
                  (let [k (fn [e] (when e (* (f e) (if scaled? (:focal e) 1.0))))]
                    (/ (hermite (k b2) (k b1) (k a1) (k a2) t) (if scaled? focal 1.0))))
                value-fns))
        :else (let [e (or b1 a1)] (mapv #(% e) value-fns))))))

(defn- first-model-entries [entries]
  (let [m (:model (first entries))] (filterv #(= m (:model %)) entries)))

(defn- pick-set
  "lensfun's choice of calibration set: the one calibrated on the closest crop
  factor that is not much larger than the camera's (crop / set crop >= 0.96)."
  [lens crop has?]
  (->> (:calibrations lens)
       (filter (fn [c] (and (has? c) (>= (/ crop (:crop c)) 0.96))))
       (sort-by #(/ crop (:crop %)))
       first))

(defn- rescale-terms
  "lensfun's rescale_polynomial_coefficients: Hugin-normalised terms to the
  focal-length unit, for calibration crop/aspect and real focal length."
  [model terms {:keys [crop aspect]} real-focal]
  (let [hugin-mm (/ sensor-diagonal crop (Math/hypot aspect 1.0) 2.0)
        hs (/ real-focal hugin-mm)
        pw (fn [x n] (Math/pow x n))]
    (case model
      :poly3 (let [d (- 1.0 (first terms))] [(* (first terms) (/ (pw hs 2) (pw d 3)))])
      :poly5 [(* (first terms) (pw hs 2)) (* (second terms) (pw hs 4))]
      :ptlens (let [[a b c] terms d (- 1.0 a b c)]
                [(* a (/ (pw hs 3) (pw d 4))) (* b (/ (pw hs 2) (pw d 3))) (* c (/ hs (pw d 2)))]))))

(defn profile
  "The :lens-profile setting for `lens` at `focal` mm on a camera with crop
  factor `crop`, or nil when the lens has no distortion calibration for that
  sensor: {:name :model :terms (rescaled) :unit :ca [kr kb] (when calibrated)}.
  `:unit` = sensor diagonal / crop / real focal length."
  [lens focal crop]
  (when-let [cset (pick-set lens crop (comp seq :distortion))]
    (let [entries (first-model-entries (:distortion cset))
          model (:model (first entries))
          n (count (:terms (first entries)))
          ;; terms and real focal length interpolated separately, as lensfun does
          terms (interpolate entries focal (mapv (fn [i] #(nth (:terms %) i)) (range n)) true)
          [real] (interpolate entries focal [:real-focal] false)
          tca-set (pick-set lens crop (comp seq :tca))
          [kr kb] (when tca-set (interpolate (:tca tca-set) focal [:kr :kb] false))]
      (cond-> {:name (str (:maker lens) " " (:model lens)) :model model
               :terms (rescale-terms model terms cset real)
               :unit (/ sensor-diagonal (double crop) real)}
        kr (assoc :ca [kr kb])))))

(defn- ratio [[n d]] (when (and n d (pos? d)) (/ (double n) (double d))))

(defn crop-factor
  "The camera's crop factor from the picture's tags (35 mm equivalent focal
  length / focal length), else `fallback`."
  [{:keys [focal-length focal-length-35mm]} fallback]
  (let [f (ratio focal-length)]
    (if (and f (pos? f) focal-length-35mm (pos? (long focal-length-35mm)))
      (/ (double focal-length-35mm) f)
      fallback)))

(defn profile-for
  "The :lens-profile setting for a picture with camera `tags`, or nil when its
  lens is not in `db`, the focal length is unknown, or no calibration fits the
  camera's sensor. Without the 35 mm-equivalent focal length in the tags the
  lens's own first calibration crop factor is assumed."
  [db tags]
  (when-let [focal (ratio (:focal-length tags))]
    (some (fn [lens] (profile lens focal (crop-factor tags (:crop (first (:calibrations lens))))))
          (find-lenses db tags))))
