(ns darkroom.ui.histogram-view
  "JavaFX histogram panel. Draws the data produced by
  darkroom.imaging.histogram/compute; does no analysis itself."
  (:import (javafx.geometry Insets)
           (javafx.scene.canvas Canvas GraphicsContext)
           (javafx.scene.control Label)
           (javafx.scene.layout VBox)
           (javafx.scene.paint Color)))

(def ^:private width 280.0)
(def ^:private height 160.0)

(def ^:private channels
  [[:r (Color/rgb 255 60 60 0.55)]
   [:g (Color/rgb 60 220 60 0.55)]
   [:b (Color/rgb 70 120 255 0.55)]])

(defn- peak
  "Scale reference: tallest bin ignoring the clipped end bins 0 and 255, so a
  few blown-out pixels don't flatten the rest of the graph."
  ^double [hist]
  (let [m (reduce (fn [m k]
                    (let [^longs a (hist k)]
                      (loop [i 1 m m] (if (< i 255) (recur (inc i) (max m (aget a i))) m))))
                  0 [:r :g :b])]
    (double (max 1 m))))

(defn- fill-channel! [^GraphicsContext gc ^longs bins ^double scale color]
  (let [step (/ width 255.0)]
    (.setFill gc ^Color color)
    (.beginPath gc)
    (.moveTo gc 0.0 height)
    (dotimes [i 256]
      (.lineTo gc (* i step) (- height (min height (* height (/ (aget bins i) scale))))))
    (.lineTo gc width height)
    (.closePath gc)
    (.fill gc)))

(defn- draw! [^Canvas canvas hist]
  (let [gc (.getGraphicsContext2D canvas)]
    (.setFill gc (Color/rgb 28 28 30))
    (.fillRect gc 0 0 width height)
    (.setStroke gc (Color/rgb 70 70 74))
    (.setLineWidth gc 1.0)
    (doseq [q [0.25 0.5 0.75]]
      (.strokeLine gc (* q width) 0 (* q width) height))
    (when hist
      (let [scale (peak hist)]
        (doseq [[k color] channels]
          (fill-channel! gc (hist k) scale color))))))

(defn create
  "Returns {:node <VBox> :update! (fn [hist])}. `update!` must be called on
  the FX thread."
  []
  (let [canvas (Canvas. width height)
        title  (doto (Label. "Histogram") (.setStyle "-fx-font-weight: bold;"))
        node   (doto (VBox. 6.0) (.setPadding (Insets. 10)))]
    (.addAll (.getChildren node) ^"[Ljavafx.scene.Node;" (into-array javafx.scene.Node [title canvas]))
    (draw! canvas nil)
    {:node node :update! #(draw! canvas %)}))
