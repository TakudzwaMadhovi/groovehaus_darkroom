(ns darkroom.ui.local-overlay
  "Mouse editing for local adjustments, drawn over the Develop canvas:

  LOCAL tab, by the selected layer's kind
    linear   drag the two end points (or drag anywhere to draw a new gradient)
    radial   drag the centre, or the side handles for the radii (or drag out a new ellipse)
    brush    paint with the mouse; size, feather, flow and erase are on the tab
    subject  drag a box around the subject
  SPOTS tab   click to place a spot (heal or clone, size on the tab)

  Everything is positioned as fractions of the cropped picture, so it stays put
  at any preview size. Shapes are redrawn from the state on every refresh."
  (:require [darkroom.imaging.local :as local]
            [darkroom.ui.state :as st]
            [darkroom.ui.theme :as theme]
            [darkroom.ui.widgets :as w])
  (:import (javafx.geometry Bounds)
           (javafx.scene Cursor Node)
           (javafx.scene.image ImageView)
           (javafx.scene.input MouseEvent)
           (javafx.scene.layout Pane)
           (javafx.scene.paint Color)
           (javafx.scene.shape Circle Ellipse Line Rectangle)))

(set! *warn-on-reflection* false)

(def ^:private grab-px 14.0)

(defn- clamp01 [v] (max 0.0 (min 1.0 (double v))))

(defn- active-mode
  "What the overlay is doing for state `s`: nil, :spots, or the selected
  layer's type."
  [s]
  (when (and (= :develop (:view s)) (:cur s) (not (:before s)))
    (case (:tab s)
      :spots :spots
      :local (:type (st/selected-layer s))
      nil)))

(defn create
  "Returns {:node :refresh! (fn [state])}. `iv` is the canvas ImageView."
  [^ImageView iv]
  (let [pane   (doto (Pane.) (.setManaged false) (.setVisible false) (.setMouseTransparent true)
                 (.setFocusTraversable false))
        _      (w/classes! pane "local-overlay")
        shapes (atom [])
        drag   (atom nil)
        size   (fn [] [(max 1.0 (.getWidth pane)) (max 1.0 (.getHeight pane))])
        frac   (fn [^MouseEvent e] (let [[pw ph] (size)] [(clamp01 (/ (.getX e) pw)) (clamp01 (/ (.getY e) ph))]))
        stroke (fn [^javafx.scene.shape.Shape sh c w] (doto sh (.setStroke c) (.setStrokeWidth (double w)) (.setMouseTransparent true)))
        handle (fn [x y] (doto (Circle. x y 6.0) (.setFill theme/bone) (.setStroke Color/BLACK) (.setStrokeWidth 1.0)
                           (.setMouseTransparent true)))
        clear! (fn [] (.removeAll (.getChildren pane) ^java.util.Collection @shapes) (reset! shapes []))
        add!   (fn [^Node n] (.add (.getChildren pane) n) (swap! shapes conj n) n)
        draw!  (fn [s]
                 (clear!)
                 (let [[pw ph] (size) mode (active-mode s) layer (st/selected-layer s) shape (:shape layer)
                       px (fn [f] (* (double f) pw)) py (fn [f] (* (double f) ph))]
                   (case mode
                     :linear
                     (let [{:keys [x0 y0 x1 y1]} shape]
                       (add! (stroke (Line. (px x0) (py y0) (px x1) (py y1)) theme/bone 1.5))
                       (add! (stroke (Line. (px x0) (py y0) (px x1) (py y1)) (theme/bone-a 0.35) 6.0))
                       (add! (handle (px x0) (py y0))) (add! (handle (px x1) (py y1))))
                     :radial
                     (let [{:keys [cx cy rx ry]} shape]
                       (add! (doto (stroke (Ellipse. (px cx) (py cy) (* rx pw) (* ry ph)) theme/bone 1.5) (.setFill Color/TRANSPARENT)))
                       (add! (handle (px cx) (py cy)))
                       (add! (handle (+ (px cx) (* rx pw)) (py cy)))
                       (add! (handle (px cx) (+ (py cy) (* ry ph)))))
                     :subject
                     (let [[x y rw rh] (:rect shape)]
                       (add! (doto (stroke (Rectangle. (px x) (py y) (* rw pw) (* rh ph)) theme/bone 1.5)
                               (.setFill Color/TRANSPARENT)
                               (.setStrokeDashOffset 0.0))))
                     :spots
                     (doseq [[i {:keys [x y r mode sx sy]}] (map-indexed vector (st/spots))]
                       (let [rad (* (double r) (max pw ph))]
                         (add! (doto (stroke (Circle. (px x) (py y) rad) (if (= mode :clone) (Color/web "#E8C24A") theme/bone) 1.5)
                                 (.setFill Color/TRANSPARENT)))
                         (when (and sx sy)
                           (add! (doto (stroke (Circle. (px sx) (py sy) rad) (theme/bone-a 0.6) 1.0)
                                   (.setFill Color/TRANSPARENT))))))
                     nil)))
        hit    (fn [shape-key [fx fy] [pw ph]]
                 (let [near? (fn [x y] (< (Math/hypot (- (* fx pw) (* (double x) pw)) (- (* fy ph) (* (double y) ph))) grab-px))]
                   (case (:type (st/selected-layer))
                     :linear (let [{:keys [x0 y0 x1 y1]} shape-key]
                               (cond (near? x0 y0) :a (near? x1 y1) :b :else nil))
                     :radial (let [{:keys [cx cy rx ry]} shape-key]
                               (cond (near? cx cy) :center (near? (+ cx rx) cy) :rx (near? cx (+ cy ry)) :ry :else nil))
                     nil)))
        set-shape! (fn [f] (let [id (:local-sel @st/state)] (st/update-layer! id #(update % :shape f))))
        brush-point! (fn [[fx fy]]
                       (let [id (:local-sel @st/state)
                             add (fn [strokes]
                                   (let [strokes (vec strokes) i (dec (count strokes))]
                                     (if (neg? i)
                                       strokes
                                       (let [{:keys [points radius]} (get strokes i)
                                             [lx ly] (last points)]
                                         (if (and lx (< (Math/hypot (- fx lx) (- fy ly)) (* 0.2 radius)))
                                           strokes
                                           (update-in strokes [i :points] conj [fx fy]))))))]
                         (st/update-layer! id (fn [l] (update-in l [:shape :strokes] add)))))]
    (.setOnMouseMoved pane
                      (w/handler
                        (fn [^MouseEvent e]
                          (.setCursor pane (case (active-mode @st/state)
                                             :spots Cursor/CROSSHAIR
                                             :brush Cursor/CROSSHAIR
                                             :subject Cursor/CROSSHAIR
                                             (if (hit (:shape (st/selected-layer)) (frac e) (size)) Cursor/HAND Cursor/CROSSHAIR))))))
    (.setOnMousePressed
      pane
      (w/handler
        (fn [^MouseEvent e]
          (let [s @st/state mode (active-mode s) p (frac e) layer (st/selected-layer s) {:keys [tool]} s]
            (case mode
              :spots (st/add-spot! {:x (first p) :y (second p) :r (:spot-size tool) :mode (:spot-mode tool) :feather 0.4 :opacity 1.0})
              :brush (do (st/update-layer!
                           (:id layer)
                           (fn [l] (update-in l [:shape :strokes] (fnil conj [])
                                              {:points [p] :radius (:brush-size tool) :feather (:brush-feather tool)
                                               :flow (:brush-flow tool) :erase? (boolean (:erase tool))})))
                         (reset! drag {:mode :brush}))
              :linear (reset! drag {:mode (or (hit (:shape layer) p (size)) :new) :start p})
              :radial (reset! drag {:mode (or (hit (:shape layer) p (size)) :new) :start p})
              :subject (reset! drag {:mode :new :start p})
              nil)))))
    (.setOnMouseDragged
      pane
      (w/handler
        (fn [^MouseEvent e]
          (when-let [{:keys [mode start]} @drag]
            (let [[fx fy] (frac e) [sx sy] start s @st/state layer (st/selected-layer s) [pw ph] (size)]
              (case (:type layer)
                :brush (brush-point! [fx fy])
                :linear (set-shape! (fn [sh] (case mode
                                               :a (assoc sh :x0 fx :y0 fy)
                                               :b (assoc sh :x1 fx :y1 fy)
                                               (assoc sh :x0 sx :y0 sy :x1 fx :y1 fy))))
                :radial (set-shape! (fn [sh] (case mode
                                               :center (assoc sh :cx fx :cy fy)
                                               :rx (assoc sh :rx (max 0.01 (Math/abs (- fx (:cx sh)))))
                                               :ry (assoc sh :ry (max 0.01 (Math/abs (- fy (:cy sh)))))
                                               (assoc sh :cx sx :cy sy :rx (max 0.01 (Math/abs (- fx sx))) :ry (max 0.01 (Math/abs (- fy sy)))))))
                :subject (set-shape! (fn [sh] (assoc sh :rect [(min sx fx) (min sy fy) (max 0.02 (Math/abs (- fx sx))) (max 0.02 (Math/abs (- fy sy)))])))
                nil))))))
    (.setOnMouseReleased
      pane
      (w/handler
        (fn [_]
          (when-let [{:keys [mode]} @drag]
            (reset! drag nil)
            (let [layer (st/selected-layer)]
              (st/commit! (str "EDIT " (get local/type-labels (:type layer) "MASK"))))))))
    (let [place! (fn []
                   (let [^Bounds b (.getBoundsInParent iv)]
                     (.setLayoutX pane (.getMinX b)) (.setLayoutY pane (.getMinY b))
                     (.resize pane (.getWidth b) (.getHeight b))
                     (draw! @st/state)))]
      (.addListener (.boundsInParentProperty iv) (w/change-listener (fn [_] (place!))))
      {:node pane
       :refresh!
       (fn [s]
         (let [mode (active-mode s) on? (and mode (not= mode :sky) (not= mode :range))]
           (.setVisible pane (boolean on?))
           (.setMouseTransparent pane (not on?))
           (when on? (place!))))})))
