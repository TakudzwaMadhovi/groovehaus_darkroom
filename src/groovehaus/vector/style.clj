(ns groovehaus.vector.style
  "Styling for JavaFX vector nodes: colours, gradients, strokes, opacity, drop shadows.

  Style map keys (all optional; only the keys present are applied, so partial updates work):
    :fill :stroke        paint spec (below); nil or \"none\" clears
    :fill-opacity :stroke-opacity   0..1, multiplied into the paint's alpha
    :stroke-width        number
    :stroke-linecap      :butt | :round | :square
    :stroke-linejoin     :miter | :round | :bevel
    :stroke-dasharray    [on off ...]
    :fill-rule           :non-zero | :even-odd   (Path only)
    :opacity             0..1 node opacity
    :effect              nil or {:type :drop-shadow :radius 8 :spread 0 :offset-x 2 :offset-y 2
                                 :color \"#00000080\"}

  Paint spec: a CSS/hex colour string (\"#ff8800\", \"#ff880080\", \"red\"), a javafx Color,
  [r g b] / [r g b a] in 0..1, or a gradient map:
    {:type :linear-gradient :from [0 0] :to [1 0] :stops [[0 \"#f00\"] [1 \"#00f\"]]
     :proportional? true :cycle :no-cycle}
    {:type :radial-gradient :center [0.5 0.5] :radius 0.5 :focus-angle 0 :focus-distance 0
     :stops [...] :proportional? true}
  Proportional gradients (the default) use 0..1 coordinates relative to the shape's bounds."
  (:import [javafx.scene Node]
           [javafx.scene.effect DropShadow]
           [javafx.scene.paint Color CycleMethod LinearGradient Paint RadialGradient Stop]
           [javafx.scene.shape Path Shape StrokeLineCap StrokeLineJoin FillRule]))

(set! *warn-on-reflection* true)

(defn- kw [v] (keyword (name v)))

(defn- dbl ^double [v]
  (cond (number? v) (double v)
        (string? v) (Double/parseDouble ^String v)
        :else (throw (ex-info "Expected a number" {:value v}))))

(defn none? [v] (or (nil? v) (and (string? v) (#{"none" ""} v))))

(defn ->color
  "Colour spec -> javafx Color."
  ^Color [c]
  (cond
    (instance? Color c) c
    (string? c) (Color/web ^String c)
    (and (sequential? c) (<= 3 (count c) 4) (every? number? c))
    (let [[r g b a] c] (Color. (dbl r) (dbl g) (dbl b) (dbl (or a 1.0))))
    :else (throw (ex-info "Unrecognised colour" {:value c}))))

(defn- with-alpha ^Color [^Color c ^double k]
  (if (== k 1.0) c (Color. (.getRed c) (.getGreen c) (.getBlue c) (* k (.getOpacity c)))))

(def ^:private cycles {:no-cycle CycleMethod/NO_CYCLE :reflect CycleMethod/REFLECT :repeat CycleMethod/REPEAT})

(defn- stops ^java.util.List [specs ^double opacity]
  (when (< (count specs) 2)
    (throw (ex-info "A gradient needs at least two stops" {:stops specs})))
  (->> specs
       (sort-by (comp dbl first))
       (mapv (fn [[o c]] (Stop. (dbl o) (with-alpha (->color c) opacity))))))

(defn ->paint
  "Paint spec -> javafx Paint (or nil for none). `opacity` scales every colour's alpha."
  (^Paint [spec] (->paint spec 1.0))
  (^Paint [spec opacity]
   (let [op (dbl opacity)]
     (cond
       (none? spec) nil
       (map? spec)
       (let [prop? (get spec :proportional? true)
             cycle (get cycles (get spec :cycle :no-cycle))]
         (when-not cycle (throw (ex-info "Unknown gradient cycle" {:cycle (:cycle spec)})))
         (case (:type spec)
           :linear-gradient
           (let [[x1 y1] (get spec :from [0 0]) [x2 y2] (get spec :to [1 0])
                 ^java.util.List st (stops (:stops spec) op)]
             (LinearGradient. (dbl x1) (dbl y1) (dbl x2) (dbl y2) (boolean prop?) ^CycleMethod cycle st))
           :radial-gradient
           (let [[cx cy] (get spec :center [0.5 0.5])
                 ^java.util.List st (stops (:stops spec) op)]
             (RadialGradient. (dbl (get spec :focus-angle 0)) (dbl (get spec :focus-distance 0))
                              (dbl cx) (dbl cy) (dbl (get spec :radius 0.5)) (boolean prop?)
                              ^CycleMethod cycle st))
           (throw (ex-info "Unknown paint type" {:spec spec}))))
       :else (with-alpha (->color spec) op)))))

(defn ->effect
  "Effect spec -> javafx Effect (currently :drop-shadow), nil for nil."
  [spec]
  (when spec
    (case (:type spec)
      :drop-shadow
      (doto (DropShadow.)
        (.setRadius (dbl (get spec :radius 8)))
        (.setSpread (dbl (get spec :spread 0)))
        (.setOffsetX (dbl (get spec :offset-x 2)))
        (.setOffsetY (dbl (get spec :offset-y 2)))
        (.setColor (->color (get spec :color "#00000080"))))
      (throw (ex-info "Unknown effect type" {:spec spec})))))

(defn- lookup [m v what]
  (or (get m (kw v)) (throw (ex-info (str "Unknown " what) {:value v :allowed (sort (keys m))}))))

(def ^:private caps {:butt StrokeLineCap/BUTT :round StrokeLineCap/ROUND :square StrokeLineCap/SQUARE})
(def ^:private joins {:miter StrokeLineJoin/MITER :round StrokeLineJoin/ROUND :bevel StrokeLineJoin/BEVEL})
(def ^:private rules {:non-zero FillRule/NON_ZERO :even-odd FillRule/EVEN_ODD})

(defn apply-style!
  "Applies a style map (see ns doc) to a node. Shape-only keys are ignored on other nodes.
  Call on the JavaFX application thread for nodes that are part of a live scene. Returns node."
  [^Node node style]
  (when (contains? style :opacity) (.setOpacity node (dbl (:opacity style))))
  (when (contains? style :effect) (.setEffect node (->effect (:effect style))))
  (when (instance? Shape node)
    (let [^Shape s node]
      (when (contains? style :fill)
        (.setFill s (->paint (:fill style) (get style :fill-opacity 1.0))))
      (when (contains? style :stroke)
        (.setStroke s (->paint (:stroke style) (get style :stroke-opacity 1.0))))
      (when (contains? style :stroke-width) (.setStrokeWidth s (dbl (:stroke-width style))))
      (when (contains? style :stroke-linecap)
        (.setStrokeLineCap s ^StrokeLineCap (lookup caps (:stroke-linecap style) "stroke-linecap")))
      (when (contains? style :stroke-linejoin)
        (.setStrokeLineJoin s ^StrokeLineJoin (lookup joins (:stroke-linejoin style) "stroke-linejoin")))
      (when (contains? style :stroke-dasharray)
        (let [da (:stroke-dasharray style)]
          (.. s getStrokeDashArray (setAll ^java.util.Collection (mapv dbl (if (none? da) [] da))))))
      (when (and (contains? style :fill-rule) (instance? Path s))
        (.setFillRule ^Path s ^FillRule (lookup rules (:fill-rule style) "fill-rule")))))
  node)
