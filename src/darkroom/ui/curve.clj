(ns darkroom.ui.curve
  "Tone-curve editor: 240x240 plot, 5 draggable points at x = 0, 1/4, 1/2, 3/4, 1,
  identity diagonal dashed, curve drawn through the same spline the engine uses."
  (:require [darkroom.imaging.develop :as develop]
            [darkroom.ui.state :as st]
            [darkroom.ui.theme :as theme]
            [darkroom.ui.widgets :as w])
  (:import (javafx.scene.input MouseEvent)
           (javafx.scene.layout Pane)
           (javafx.scene.shape Circle Line MoveTo LineTo Path Rectangle StrokeType)))

(set! *warn-on-reflection* false)

(def ^:private off 8.0)   ; plot margin so end points are not clipped
(def ^:private size 240.0)

(defn- clamp01 [v] (max 0.0 (min 1.0 (double v))))

(defn create
  "Returns {:node :sync! (fn [adj])}."
  []
  (let [pane   (doto (Pane.) (.setMinSize 256 256) (.setPrefSize 256 256) (.setMaxSize 256 256))
        frame  (doto (Rectangle. off off size size) (.setFill nil) (.setStroke (theme/bone-a 0.18)))
        grid   (doto (Path.) (.setStroke (theme/bone-a 0.10)) (.setStrokeWidth 1.0))
        diag   (doto (Line. off (+ off size) (+ off size) off)
                 (.setStroke (theme/bone-a 0.18)))
        curve  (doto (Path.) (.setStroke theme/bone) (.setStrokeWidth 1.5) (.setFill nil))
        pts    (vec (repeatedly 5 #(doto (Circle. 5.0) (.setFill theme/black) (.setStroke theme/sun) (.setStrokeWidth 1.5))))
        dragging (atom false)
        sync!  (fn [adj]
                 (let [cv  (get adj :curve develop/default-curve)
                       lut (develop/curve-lut cv)]
                   (.clear (.getElements curve))
                   (dotimes [i 41]
                     (let [x (/ i 40.0)
                           px (+ off (* x size))
                           py (+ off (* (- 1.0 (aget lut (Math/round (* x 255.0)))) size))]
                       (.add (.getElements curve) (if (zero? i) (MoveTo. px py) (LineTo. px py)))))
                   (dotimes [i 5]
                     (doto (nth pts i)
                       (.setCenterX (+ off (* i 60.0)))
                       (.setCenterY (+ off (* (- 1.0 (double (nth cv i))) size)))))))
        at     (fn [^MouseEvent e]
                 (let [x (/ (- (.getX e) off) size)
                       y (- 1.0 (/ (- (.getY e) off) size))
                       i (max 0 (min 4 (Math/round (* x 4.0))))
                       cv (vec (get (st/cur-adj @st/state) :curve develop/default-curve))]
                   (st/set-adj! :curve (assoc cv i (/ (Math/round (* 100.0 (clamp01 y))) 100.0)))))]
    (doseq [q [0.25 0.5 0.75]]
      (.setStyle grid "")
      (doto (.getElements grid)
        (.add (MoveTo. (+ off (* q size)) off)) (.add (LineTo. (+ off (* q size)) (+ off size)))
        (.add (MoveTo. off (+ off (* q size)))) (.add (LineTo. (+ off size) (+ off (* q size))))))
    (.addAll (.getStrokeDashArray diag) (java.util.Arrays/asList (into-array Double [3.0 4.0])))
    (apply w/add! pane frame grid diag curve pts)
    (.setStyle pane "-fx-cursor: crosshair;")
    (.setOnMousePressed pane (w/handler (fn [e] (reset! dragging true) (at e))))
    (.setOnMouseDragged pane (w/handler (fn [e] (when @dragging (at e)))))
    (.setOnMouseReleased pane (w/handler (fn [_] (when @dragging (reset! dragging false) (st/commit! "TONE CURVE")))))
    (sync! {})
    {:node pane :sync! sync!}))
