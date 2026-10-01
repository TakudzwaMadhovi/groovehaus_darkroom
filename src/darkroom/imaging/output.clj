(ns darkroom.imaging.output
  "Everything between a developed picture and a file: file-name templates,
  output sharpening, a text watermark, metadata choices, and rendering one or
  many frames to disk. Pure logic, no UI dependency.

  Export options (a map; see `defaults`):
    :dir :template :format (:jpeg :png :tiff :webp) :quality (0-1] :size (long edge px, 0 = full)
    :space (:srgb :display-p3 :adobe-rgb)
    :sharpen  nil or {:for :screen|:matte|:glossy :amount :low|:standard|:high}
    :metadata :all (camera data, copyright, creator) | :copyright (only those) | :none
    :watermark nil or {:text :position :opacity :size}"
  (:require [clojure.string :as str]
            [darkroom.catalog :as cat]
            [darkroom.imaging.detail :as detail]
            [darkroom.imaging.exif :as exif]
            [darkroom.imaging.export :as export]
            [darkroom.imaging.geometry :as geometry]
            [darkroom.imaging.loader :as loader]
            [darkroom.imaging.pipeline :as pipeline])
  (:import (java.awt Color Font RenderingHints)
           (java.awt.image BufferedImage)
           (java.io File)
           (java.time LocalDate)))

(def defaults
  {:template "groovehaus_{name}" :format :jpeg :quality 0.9 :size 2048 :space :srgb
   :sharpen nil :metadata :all :watermark nil})

;; ---------------------------------------------------------- file names

(defn expand-template
  "Fills a file-name template. Tokens: {name} the file's name, {n} the position
  in this export (from 1; {n3} padded to three digits), {date} the capture date
  (else today) as YYYY-MM-DD, {rating} 0-5, {shoot} the shoot's name. Unknown
  tokens are left as written. Path separators and leading dots are removed, so a
  template can never leave the destination folder."
  [template {:keys [name n date rating shoot]}]
  (let [vals {"name" name "n" (str n) "n3" (format "%03d" (long (or n 0))) "date" date
              "rating" (str (or rating 0)) "shoot" shoot}
        out (str/replace (str template) #"\{(\w+)\}" (fn [[all k]] (str (get vals k all))))
        out (-> out (str/replace #"[/\\:*?\"<>|]" "_") (str/replace #"^\.+" "") str/trim)]
    (if (str/blank? out) (str name) out)))

(defn capture-date
  "YYYY-MM-DD from EXIF tags (:datetime-original \"2024:05:01 10:00:00\"), else today."
  [tags]
  (if-let [[_ y m d] (re-find #"^(\d{4}):(\d{2}):(\d{2})" (str (:datetime-original tags)))]
    (str y "-" m "-" d)
    (str (LocalDate/now))))

(defn free-name
  "First free `base.ext`, `base-2.ext`, ... in `dir`: never overwrites a file."
  [dir base fmt]
  (loop [n 1]
    (let [name (if (= n 1) base (str base "-" n))]
      (if (.exists (export/target-file dir name fmt))
        (recur (inc n))
        name))))

;; ------------------------------------------------------ output sharpening

(def sharpen-media
  "Sharpening for the destination: unsharp radius in output pixels and amount
  (0-100, same scale as the SHARPEN slider) for LOW / STANDARD / HIGH. Rougher
  paper hides more, so matte and glossy prints get more than a screen."
  {:screen  {:radius 0.7 :amounts {:low 25 :standard 40 :high 60}}
   :matte   {:radius 1.1 :amounts {:low 35 :standard 55 :high 80}}
   :glossy  {:radius 1.3 :amounts {:low 45 :standard 65 :high 95}}})

(defn sharpen-output
  "Sharpens the already-resized scene `img` for its destination (luminance only,
  half of it held back from flat areas). nil `spec` leaves it alone."
  [img {:keys [for amount] :as spec}]
  (if-not spec
    img
    (let [{:keys [radius amounts]} (or (sharpen-media for) (sharpen-media :screen))]
      (detail/local-contrast img {:sharpen (get amounts (or amount :standard) 40) :sharpen-radius radius :sharpen-masking 0.4}
                             {:scale 1.0}))))

;; ---------------------------------------------------------- watermark

(def watermark-positions [:bottom-right :bottom-left :top-right :top-left :center])

(defn- text-mask
  "[mask w h] of `text` drawn in `font`: a float array of coverage 0-1."
  [^String text ^Font font]
  (let [probe (doto (.createGraphics (BufferedImage. 1 1 BufferedImage/TYPE_BYTE_GRAY))
                (.setFont font))
        fm (.getFontMetrics probe)
        w (max 1 (+ 2 (.stringWidth fm text))) h (max 1 (+ 2 (.getHeight fm)))
        buf (BufferedImage. w h BufferedImage/TYPE_BYTE_GRAY)
        g (.createGraphics buf)]
    (.dispose probe)
    (doto g
      (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_ON)
      (.setFont font) (.setColor Color/WHITE)
      (.drawString text 1 (+ 1 (.getAscent fm)))
      (.dispose))
    (let [px (.getData ^java.awt.image.DataBufferByte (.getDataBuffer (.getRaster buf)))]
      [(float-array (map #(/ (bit-and (long %) 255) 255.0) px)) w h])))

(defn watermark
  "Draws `text` over the scene image `img`: white with a soft dark edge, `size`
  (fraction of the long edge, default 0.03) tall, at `position`, at `opacity`
  (0-1, default 0.6). Blank text returns `img` untouched."
  [{:keys [^long width ^long height data] :as img} {:keys [text position opacity size]}]
  (if (str/blank? text)
    img
    (let [long-e (max width height)
          px (max 8 (Math/round (* (double (or size 0.03)) long-e)))
          [^floats mask mw mh] (text-mask (str/trim text) (Font. Font/SANS_SERIF Font/BOLD (int px)))
          mw (long mw) mh (long mh)
          margin (Math/round (* 0.02 long-e))
          x0 (long (case position
                     (:bottom-left :top-left) margin
                     :center (quot (- width mw) 2)
                     (- width mw margin)))
          y0 (long (case position
                     (:top-left :top-right) margin
                     :center (quot (- height mh) 2)
                     (- height mh margin)))
          op (double (or opacity 0.6))
          shadow (max 1 (quot px 14))
          ^floats src data
          ^floats out (aclone src)
          cov (fn ^double [^long x ^long y] (if (and (< -1 x mw) (< -1 y mh)) (double (aget mask (+ (* y mw) x))) 0.0))]
      (doseq [y (range (max 0 y0) (min height (+ y0 mh shadow 1)))
              x (range (max 0 x0) (min width (+ x0 mw shadow 1)))]
        (let [tx (- x x0) ty (- y y0)
              a  (* op (cov tx ty))
              sh (* op 0.5 (cov (- tx shadow) (- ty shadow)))
              j  (* 3 (+ (* y width) x))]
          (dotimes [c 3]
            (let [v (double (aget src (+ j c)))
                  v (* v (- 1.0 sh))                 ; shadow darkens
                  v (+ (* v (- 1.0 a)) a)]           ; text lightens toward white
              (aset out (+ j c) (float v))))))
      (assoc img :data out))))

;; --------------------------------------------------------------- metadata

(defn export-tags
  "EXIF tags to write for `mode` (:all :copyright :none) from the source file's
  `tags`, with the frame's own :creator / :copyright (catalog notes) taking
  precedence over what the camera recorded."
  [mode tags {:keys [creator copyright]}]
  (let [mine (cond-> {} (not (str/blank? creator)) (assoc :artist creator)
                        (not (str/blank? copyright)) (assoc :copyright copyright))]
    (case mode
      :none nil
      :copyright (merge (select-keys tags [:artist :copyright]) mine)
      (merge tags mine))))

;; -------------------------------------------------------------- rendering

(defn render-frame
  "The finished scene image for a frame: developed with `adj` at full quality,
  resized to the long edge, sharpened for the destination, watermarked."
  [path adj {:keys [size sharpen] :as opts}]
  (let [img (-> (pipeline/render (loader/load-scene path adj) adj {:quality :final})
                (geometry/resize-long-edge (when (pos? (long (or size 0))) size))
                (sharpen-output sharpen))]
    (if-let [spec (:watermark opts)] (watermark img spec) img)))

(defn export-frame!
  "Renders one frame and writes it. `frame` {:path :adj :n :shoot :rating :meta}
  (:meta = catalog notes {:creator :copyright}); `opts` as in the namespace
  docstring. Returns the File written (a numbered name if the file exists)."
  [{:keys [path adj n shoot rating] :as frame} opts]
  (let [{:keys [dir template format quality space metadata] :as o} (merge defaults opts)
        space (if (= format :webp) :srgb space)
        tags  (exif/read-tags path)
        base  (expand-template template {:name (str/lower-case (cat/frame-name path)) :n (or n 1)
                                         :date (capture-date tags) :rating rating :shoot shoot})
        name  (free-name dir base format)
        out   (render-frame path adj o)]
    (export/save-scene! out {:dir dir :name name :format format :quality quality :space space
                             :tags (export-tags metadata tags (:meta frame))})))

(defn export-frames!
  "Exports each frame in order, numbering them 1.. for {n}. `progress` is called
  with (done total) after each. A frame that fails is skipped. Returns
  {:files [File] :failed [[path message]]}."
  [frames opts progress]
  (let [total (count frames)]
    (loop [[f & more] frames i 1 files [] failed []]
      (if-not f
        {:files files :failed failed}
        (let [r (try {:file (export-frame! (assoc f :n i) opts)}
                     (catch Throwable t {:error [(:path f) (.getMessage t)]}))]
          (when progress (progress i total))
          (recur more (inc i) (cond-> files (:file r) (conj (:file r))) (cond-> failed (:error r) (conj (:error r)))))))))
