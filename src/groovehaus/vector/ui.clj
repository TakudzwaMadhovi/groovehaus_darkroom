(ns groovehaus.vector.ui
  "Vector overlay layer that sits on top of an ImageView.

  (def overlay (ui/make-overlay))
  (ui/mount! stack-pane image-view overlay)        ; adds the overlay pane above the ImageView
  (ui/add-shape! overlay :cutout [:path {:d \"M0 0 L100 0 L50 80 Z\"}]
                 {:style {:fill {:type :linear-gradient :stops [[0 \"#ff0\"] [1 \"#f0f\"]]}
                          :stroke \"#fff\" :stroke-width 3
                          :effect {:type :drop-shadow :radius 12}}})
  (ui/set-style! overlay :cutout {:opacity 0.8})

  Shape coordinates are IMAGE PIXELS: the content group is scaled by (displayed size / image size),
  so a path traced from a mask lines up with the photo at any zoom/fit. The overlay pane is
  transparent, clipped to the image area and mouse-transparent by default.

  Threading: functions here touch live scene-graph nodes, so call them on the JavaFX application
  thread (see `run-fx!`). Limits: the ImageView viewport (cropping/panning inside the view) is not
  accounted for; the overlay tracks fitWidth/fitHeight/preserveRatio sizing only."
  (:require [groovehaus.vector.core :as vc]
            [groovehaus.vector.style :as style])
  (:import [java.util.concurrent Callable]
           [javafx.application Platform]
           [javafx.beans Observable]
           [javafx.beans.binding Bindings DoubleBinding]
           [javafx.beans.value ObservableDoubleValue ObservableValue]
           [javafx.scene Group Node]
           [javafx.scene.image ImageView]
           [javafx.scene.layout Pane StackPane]
           [javafx.scene.shape Rectangle]
           [javafx.scene.transform Scale]))

(set! *warn-on-reflection* true)

(defn run-fx!
  "Runs f on the FX thread (immediately if already on it)."
  [f]
  (if (Platform/isFxApplicationThread) (f) (Platform/runLater f)))

(defn fit-scale
  "Scale from image pixels to displayed pixels along one axis; 1.0 while sizes are unknown."
  ^double [^double view-size ^double image-size]
  (if (and (pos? view-size) (pos? image-size)) (/ view-size image-size) 1.0))

(defn make-overlay
  "Creates an overlay: {:pane Pane :content Group :scale Scale :shapes (atom {id node})}."
  []
  (let [pane (doto (Pane.)
               (.setMouseTransparent true)
               (.setPickOnBounds false)
               (.setStyle "-fx-background-color: transparent;"))
        scale (Scale. 1.0 1.0 0.0 0.0)
        content (doto (Group.) (.setManaged false))
        clip (Rectangle.)]
    (.add (.getTransforms content) scale)
    (.add (.getChildren pane) content)
    (.bind (.widthProperty clip) (.widthProperty pane))
    (.bind (.heightProperty clip) (.heightProperty pane))
    (.setClip pane clip)
    {:pane pane :content content :scale scale :shapes (atom {})}))

(defn- double-binding ^DoubleBinding [f deps]
  (Bindings/createDoubleBinding (reify Callable (call [_] (double (f)))) (into-array Observable deps)))

(defn bind-size!
  "Keeps the overlay sized to the displayed image and its content scaled from image pixels.
  All four arguments are ObservableValue<Number>s (view width/height on screen, image width/height
  in pixels). Separated from ImageView so it works with any source (and headless tests)."
  [overlay ^ObservableValue view-w ^ObservableValue view-h ^ObservableValue img-w ^ObservableValue img-h]
  (let [^Pane pane (:pane overlay) ^Scale scale (:scale overlay)
        num (fn [^ObservableValue o] (let [v (.getValue o)] (if v (.doubleValue ^Number v) 0.0)))
        sx (double-binding #(fit-scale (num view-w) (num img-w)) [view-w img-w])
        sy (double-binding #(fit-scale (num view-h) (num img-h)) [view-h img-h])]
    (doto pane
      (.setMinSize 0 0))
    (.bind (.prefWidthProperty pane) ^ObservableDoubleValue (double-binding #(num view-w) [view-w]))
    (.bind (.prefHeightProperty pane) ^ObservableDoubleValue (double-binding #(num view-h) [view-h]))
    (.bind (.maxWidthProperty pane) (.prefWidthProperty pane))
    (.bind (.maxHeightProperty pane) (.prefHeightProperty pane))
    (.bind (.xProperty scale) sx)
    (.bind (.yProperty scale) sy)
    overlay))

(defn mount!
  "Binds the overlay to an ImageView and adds its pane to `stack-pane` above it. The ImageView and
  overlay must share the StackPane's (centre) alignment, which is the StackPane default."
  [^StackPane stack-pane ^ImageView image-view overlay]
  (let [^ObservableValue img-prop (.imageProperty image-view)
        ^"[Ljava.lang.String;" w-path (into-array String ["width"])
        ^"[Ljava.lang.String;" h-path (into-array String ["height"])
        image-w (Bindings/selectDouble img-prop w-path)
        image-h (Bindings/selectDouble img-prop h-path)
        bounds (.layoutBoundsProperty image-view)
        view-w (double-binding #(.getWidth (.getLayoutBounds image-view)) [bounds])
        view-h (double-binding #(.getHeight (.getLayoutBounds image-view)) [bounds])]
    (bind-size! overlay view-w view-h image-w image-h)
    (let [children (.getChildren stack-pane)]
      (when-not (.contains children (:pane overlay))
        (.add children (:pane overlay))))
    overlay))

;; ---- shape management --------------------------------------------------------

(defn- ->node ^Node [spec]
  (cond
    (instance? Node spec) spec
    (vector? spec) (vc/hiccup->node spec)
    (and (string? spec) (.startsWith (.trim ^String spec) "<")) (vc/svg->node spec)
    :else (throw (ex-info "Shape spec must be a hiccup vector, SVG string or Node" {:spec spec}))))

(defn remove-shape! [overlay id]
  (when-let [node (get @(:shapes overlay) id)]
    (.remove (.getChildren ^Group (:content overlay)) node)
    (swap! (:shapes overlay) dissoc id))
  overlay)

(defn add-shape!
  "Adds (or replaces, keeping z-order at the top) a shape under `id`. `spec` is hiccup, an SVG
  string, or a JavaFX Node. opts: {:style style-map} applied after the shape's own attributes."
  ([overlay id spec] (add-shape! overlay id spec nil))
  ([overlay id spec {:keys [style]}]
   (let [node (->node spec)]
     (remove-shape! overlay id)
     (when style (style/apply-style! node style))
     (.add (.getChildren ^Group (:content overlay)) node)
     (swap! (:shapes overlay) assoc id node)
     node)))

(defn add-path!
  "Convenience: adds a path from an SVG `d` string (e.g. from groovehaus.vector.trace)."
  ([overlay id d] (add-path! overlay id d nil))
  ([overlay id d opts] (add-shape! overlay id [:path {:d d}] opts)))

(defn shape [overlay id] (get @(:shapes overlay) id))

(defn shape-ids [overlay]
  (let [shapes @(:shapes overlay)]
    (->> (.getChildren ^Group (:content overlay))
         (keep (fn [n] (some (fn [[id node]] (when (identical? node n) id)) shapes))))))

(defn set-style!
  "Applies a (partial) style map to an existing shape."
  [overlay id style]
  (if-let [node (shape overlay id)]
    (style/apply-style! node style)
    (throw (ex-info "No such shape" {:id id :known (shape-ids overlay)})))
  overlay)

(defn set-visible! [overlay id visible?]
  (let [^Node node (or (shape overlay id) (throw (ex-info "No such shape" {:id id})))]
    (.setVisible node (boolean visible?)))
  overlay)

(defn clear! [overlay]
  (.clear (.getChildren ^Group (:content overlay)))
  (reset! (:shapes overlay) {})
  overlay)
