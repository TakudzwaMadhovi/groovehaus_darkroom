(ns groovehaus.vector.core
  "SVG/hiccup -> JavaFX shape nodes.

  Supported elements: svg g path rect circle ellipse line polyline polygon (defs/title/desc/
  metadata are ignored). Supported attributes: geometry attrs, id, transform (translate scale
  rotate skewX skewY matrix), fill stroke stroke-width fill-opacity stroke-opacity opacity
  fill-rule stroke-linecap stroke-linejoin stroke-dasharray, `style=\"a:b;c:d\"`, plus the
  non-SVG :effect (see groovehaus.vector.style). Presentation attrs inherit through <g>/<svg>
  as in SVG. Not supported: gradients/patterns via url(#id), clipPath, text, CSS classes, use.

  Hiccup form:
    [:svg {:viewBox \"0 0 100 100\" :width 100 :height 100}
      [:path {:d \"M10 10 L90 10 L50 90 Z\" :fill \"#f80\" :stroke \"#000\"}]
      [:g {:transform \"translate(5 5)\"} [:circle {:cx 20 :cy 20 :r 8}]]]

  Coordinates are plain numbers in the vector layer's space, which the overlay maps onto image
  pixels. Nodes can be built off the FX thread but must be mutated on it once shown."
  (:require [clojure.string :as str]
            [groovehaus.vector.path :as path]
            [groovehaus.vector.style :as style])
  (:import [javafx.scene Group Node]
           [javafx.scene.shape ArcTo ClosePath CubicCurveTo Circle Ellipse Line LineTo MoveTo Path
            PathElement Polygon Polyline QuadCurveTo Rectangle Shape]
           [javafx.scene.transform Affine Rotate Scale Translate]
           [javax.xml.parsers DocumentBuilderFactory]
           [org.w3c.dom Element]
           [java.io StringReader]
           [org.xml.sax ErrorHandler InputSource SAXParseException]))

(set! *warn-on-reflection* true)

(def parse-path "See groovehaus.vector.path/parse-path." path/parse-path)

;; ---- path -> JavaFX -------------------------------------------------------

(defn segment->element
  "One absolute segment (from parse-path) -> PathElement."
  ^PathElement [{:keys [op x y x1 y1 x2 y2 rx ry rotation large? sweep?]}]
  (case op
    :move (MoveTo. (double x) (double y))
    :line (LineTo. (double x) (double y))
    :quad (QuadCurveTo. (double x1) (double y1) (double x) (double y))
    :cubic (CubicCurveTo. (double x1) (double y1) (double x2) (double y2) (double x) (double y))
    :arc (ArcTo. (double rx) (double ry) (double rotation) (double x) (double y)
                 (boolean large?) (boolean sweep?))
    :close (ClosePath.)))

(defn segments->path
  "Segments -> javafx.scene.shape.Path."
  ^Path [segs]
  (let [p (Path.)]
    (.addAll (.getElements p) ^java.util.Collection (mapv segment->element segs))
    p))

(defn path-d->fx
  "SVG `d` string -> javafx.scene.shape.Path (MoveTo/LineTo/CubicCurveTo/QuadCurveTo/ArcTo/ClosePath)."
  ^Path [^String d]
  (segments->path (path/parse-path d)))

;; ---- attribute helpers ----------------------------------------------------

(defn- dbl ^double [v default]
  (cond (nil? v) (double default)
        (number? v) (double v)
        :else (Double/parseDouble (str/trim (str v)))))

(defn- numbers [s]
  (mapv #(Double/parseDouble ^String %) (re-seq #"[+-]?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?" (str s))))

(defn- parse-transform [s]
  (vec
   (for [[_ fname args] (re-seq #"(\w+)\s*\(([^)]*)\)" (str s))
         :let [a (numbers args)
               bad #(throw (ex-info (str "Bad transform: " fname "(" args ")") {:transform s}))]]
     (case fname
       "translate" (case (count a) 1 (Translate. (a 0) 0) 2 (Translate. (a 0) (a 1)) (bad))
       "scale" (case (count a) 1 (Scale. (a 0) (a 0)) 2 (Scale. (a 0) (a 1)) (bad))
       "rotate" (case (count a) 1 (Rotate. (a 0)) 3 (Rotate. (a 0) (a 1) (a 2)) (bad))
       "skewX" (Affine. 1.0 (Math/tan (Math/toRadians (a 0))) 0.0 0.0 1.0 0.0)
       "skewY" (Affine. 1.0 0.0 0.0 (Math/tan (Math/toRadians (a 0))) 1.0 0.0)
       ;; SVG matrix(a b c d e f): x' = ax+cy+e, y' = bx+dy+f
       "matrix" (if (= 6 (count a)) (Affine. (a 0) (a 2) (a 4) (a 1) (a 3) (a 5)) (bad))
       (bad)))))

(def ^:private inheritable
  #{:fill :stroke :stroke-width :fill-opacity :stroke-opacity :fill-rule
    :stroke-linecap :stroke-linejoin :stroke-dasharray})

(def ^:private style-keys (into #{:opacity :effect} inheritable))

(defn- parse-style-attr [s]
  (into {} (for [decl (str/split (str s) #";") :let [[k v] (map str/trim (str/split decl #":" 2))]
                 :when (and k v (seq k))]
             [(keyword k) v])))

(defn- attrs->style [attrs]
  (let [dash (:stroke-dasharray attrs)
        m (merge (select-keys attrs style-keys)
                 (select-keys (parse-style-attr (:style attrs)) style-keys))]
    (cond-> m
      (string? (:stroke-dasharray m)) (update :stroke-dasharray #(if (style/none? %) nil (numbers %))))))

(defn- numeric-style [m]
  (cond-> m
    (string? (:stroke-width m)) (update :stroke-width #(Double/parseDouble ^String %))
    (string? (:opacity m)) (update :opacity #(Double/parseDouble ^String %))
    (string? (:fill-opacity m)) (update :fill-opacity #(Double/parseDouble ^String %))
    (string? (:stroke-opacity m)) (update :stroke-opacity #(Double/parseDouble ^String %))))

;; ---- hiccup -> nodes --------------------------------------------------------

(defn- split-hiccup [form]
  (when-not (and (vector? form) (keyword? (first form)))
    (throw (ex-info "Expected hiccup vector like [:path {...}]" {:form form})))
  (let [[tag & more] form]
    (if (map? (first more))
      [tag (first more) (rest more)]
      [tag {} more])))

(defn- points->doubles [s]
  (let [ns (numbers s)]
    (when (odd? (count ns)) (throw (ex-info "points needs an even number of values" {:points s})))
    (double-array ns)))

(defn- view-box-transforms [attrs]
  (when-let [vb (:viewBox attrs)]
    (let [[vx vy vw vh] (numbers vb)
          w (some-> (:width attrs) (dbl 0)) h (some-> (:height attrs) (dbl 0))]
      (when (and w h (pos? vw) (pos? vh))
        ;; preserveAspectRatio default: xMidYMid meet
        (let [s (min (/ w vw) (/ h vh))]
          [(Translate. (- (/ (- w (* vw s)) 2.0) (* vx s)) (- (/ (- h (* vh s)) 2.0) (* vy s)))
           (Scale. s s)])))))

(declare build)

(defn- build-children [kids style opts]
  (keep #(build % style opts) (mapcat #(if (and (sequential? %) (not (keyword? (first %)))) % [%]) kids)))

(defn- finish! [^Node node attrs style]
  (when-let [id (:id attrs)] (.setId node (str id)))
  (when (:transform attrs) (.addAll (.getTransforms node) ^java.util.Collection (parse-transform (:transform attrs))))
  (style/apply-style! node style))

(defn- build
  "form + inherited style -> Node (or nil for ignored elements)."
  [form inherited opts]
  (if (instance? Node form)
    form
    (let [[tag attrs kids] (split-hiccup form)
          own (numeric-style (attrs->style attrs))
          inh (merge inherited (select-keys own inheritable))
          shape-style (merge {:stroke nil} inh (select-keys own [:opacity :effect]))
          shape (fn [^Shape s] (finish! s attrs shape-style) s)]
      (case tag
        (:defs :title :desc :metadata) nil
        (:svg :g)
        (let [g (Group.)]
          (.addAll (.getChildren g) ^java.util.Collection (vec (build-children kids inh opts)))
          (when (= tag :svg) (.addAll (.getTransforms g) ^java.util.Collection (vec (view-box-transforms attrs))))
          (finish! g attrs (select-keys own [:opacity :effect]))
          g)
        :path (shape (path-d->fx (or (:d attrs) "")))
        :rect (let [x (dbl (:x attrs) 0) y (dbl (:y attrs) 0)
                    rx (:rx attrs) ry (:ry attrs)
                    rx' (dbl (or rx ry) 0) ry' (dbl (or ry rx) 0)
                    r (Rectangle. x y (dbl (:width attrs) 0) (dbl (:height attrs) 0))]
                (when (or (pos? rx') (pos? ry')) ; JavaFX arc sizes are diameters
                  (.setArcWidth r (* 2 rx')) (.setArcHeight r (* 2 ry')))
                (shape r))
        :circle (shape (Circle. (dbl (:cx attrs) 0) (dbl (:cy attrs) 0) (dbl (:r attrs) 0)))
        :ellipse (shape (Ellipse. (dbl (:cx attrs) 0) (dbl (:cy attrs) 0) (dbl (:rx attrs) 0) (dbl (:ry attrs) 0)))
        :line (let [l (Line. (dbl (:x1 attrs) 0) (dbl (:y1 attrs) 0) (dbl (:x2 attrs) 0) (dbl (:y2 attrs) 0))]
                (shape l))
        :polyline (let [p (Polyline. (points->doubles (:points attrs)))] (shape p))
        :polygon (let [p (Polygon. (points->doubles (:points attrs)))] (shape p))
        (if (false? (:strict? opts))
          nil
          (throw (ex-info (str "Unsupported element: " tag) {:tag tag})))))))

(defn hiccup->node
  "Hiccup vector (or an existing Node) -> JavaFX Node. Unsupported elements throw unless
  opts has :strict? false, in which case they are skipped."
  (^Node [form] (hiccup->node form nil))
  (^Node [form opts]
   (or (build form {:fill "#000000"} opts)
       (throw (ex-info "Element produced no node" {:form form})))))

;; ---- XML ------------------------------------------------------------------

(defn- local-name [^String s] (keyword (last (str/split s #":"))))

(defn- element->hiccup [^Element e]
  (let [attrs (let [nm (.getAttributes e)]
                (into {} (for [i (range (.getLength nm))
                               :let [a (.item nm i) k (.getNodeName a)]
                               :when (not (str/starts-with? k "xmlns"))
                               :when (not (str/includes? k ":"))]
                           [(keyword k) (.getNodeValue a)])))
        kids (let [cn (.getChildNodes e)]
               (for [i (range (.getLength cn))
                     :let [c (.item cn i)]
                     :when (instance? Element c)]
                 (element->hiccup c)))]
    (into [(local-name (.getTagName e)) attrs] kids)))

(defn svg->hiccup
  "Parses an SVG document string into hiccup. DOCTYPEs (and so external entities) are rejected."
  [^String svg]
  (let [f (doto (DocumentBuilderFactory/newInstance)
            (.setFeature "http://apache.org/xml/features/disallow-doctype-decl" true)
            (.setXIncludeAware false)
            (.setExpandEntityReferences false))
        builder (doto (.newDocumentBuilder f)
                  (.setErrorHandler (reify ErrorHandler
                                      (warning [_ _])
                                      (error [_ e] (throw e))
                                      (fatalError [_ e] (throw e)))))
        doc (.parse builder (InputSource. (StringReader. svg)))]
    (element->hiccup (.getDocumentElement doc))))

(defn svg->node
  "SVG document string -> JavaFX Node."
  (^Node [svg] (svg->node svg nil))
  (^Node [svg opts] (hiccup->node (svg->hiccup svg) opts)))
