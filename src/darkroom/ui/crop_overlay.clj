(ns darkroom.ui.crop-overlay
  "The interactive crop rectangle, drawn over the Develop canvas while the CROP
  tab is open (the canvas then shows the whole frame; see canvas/render-settings).

  Drag inside the rectangle to move it, drag a corner or edge to resize it, drag
  outside it to draw a new one. The ASPECT pills (CROP tab) hold a ratio while
  dragging; FREE releases it. The maths is in darkroom.imaging.crop; the same
  rectangle can be set precisely with the sliders on the tab."
  (:require [darkroom.imaging.crop :as crop]
            [darkroom.imaging.geometry :as geometry]
            [darkroom.ui.state :as st]
            [darkroom.ui.theme :as theme]
            [darkroom.ui.widgets :as w])
  (:import (javafx.geometry Bounds)
           (javafx.scene Cursor)
           (javafx.scene.image ImageView)
           (javafx.scene.input MouseEvent)
           (javafx.scene.layout Pane)
           (javafx.scene.paint Color)
           (javafx.scene.shape Line Rectangle)))

(set! *warn-on-reflection* false)

(def ^:private handle-px 10.0)
(def ^:private grab-px 12.0)

(def ^:private cursors
  {:nw Cursor/NW_RESIZE :ne Cursor/NE_RESIZE :se Cursor/SE_RESIZE :sw Cursor/SW_RESIZE
   :n Cursor/N_RESIZE :s Cursor/S_RESIZE :w Cursor/W_RESIZE :e Cursor/E_RESIZE
   :move Cursor/MOVE})

(defn lock-of
  "The fraction-space ratio the crop must keep for aspect setting `aspect`
  (\"free\", \"orig\" or a preset such as \"4:5\") on a frame of `frame-aspect`."
  [aspect frame-aspect]
  (crop/lock-ratio (cond (= aspect "free") :free
                         (= aspect "orig") :orig
                         (geometry/aspect-ratios aspect) (geometry/aspect-ratios aspect)
                         :else :free)
                   frame-aspect))

(defn create
  "Returns {:node :refresh! (fn [state])}. `iv` is the canvas ImageView; the
  overlay sits exactly over the image it displays."
  [^ImageView iv]
  (let [pane    (doto (Pane.) (.setManaged false) (.setVisible false) (.setMouseTransparent true)
                  (.setFocusTraversable false))
        _       (w/classes! pane "crop-overlay")
        dim-fill (Color/rgb 0 0 0 0.55)
        dims    (vec (repeatedly 4 #(doto (Rectangle.) (.setFill dim-fill) (.setMouseTransparent true))))
        border  (doto (Rectangle.) (.setFill Color/TRANSPARENT) (.setStroke theme/bone) (.setStrokeWidth 1.5)
                  (.setMouseTransparent true))
        grid    (vec (repeatedly 4 #(doto (Line.) (.setStroke (theme/bone-a 0.35)) (.setStrokeWidth 1.0)
                                      (.setMouseTransparent true))))
        handles (into {} (for [k [:nw :n :ne :e :se :s :sw :w]]
                           [k (doto (Rectangle. handle-px handle-px) (.setFill theme/bone) (.setStroke Color/BLACK)
                                (.setStrokeWidth 1.0) (.setMouseTransparent true))]))
        rect    (atom [0.0 0.0 1.0 1.0])      ; [x y w h] as fractions of the frame
        aspect  (atom "orig")
        drag    (atom nil)
        size    (fn [] [(max 1.0 (.getWidth pane)) (max 1.0 (.getHeight pane))])
        frac    (fn [^MouseEvent e] (let [[pw ph] (size)] [(/ (.getX e) pw) (/ (.getY e) ph)]))
        lock    (fn [] (let [[pw ph] (size)] (lock-of @aspect (/ pw ph))))
        draw!   (fn []
                  (let [[pw ph] (size) [x y rw rh] @rect
                        px (* x pw) py (* y ph) cw (* rw pw) ch (* rh ph)
                        set-rect! (fn [^Rectangle r a b c d] (doto r (.setX a) (.setY b) (.setWidth (max 0.0 c)) (.setHeight (max 0.0 d))))]
                    (set-rect! (dims 0) 0 0 pw py)
                    (set-rect! (dims 1) 0 (+ py ch) pw (- ph py ch))
                    (set-rect! (dims 2) 0 py px ch)
                    (set-rect! (dims 3) (+ px cw) py (- pw px cw) ch)
                    (set-rect! border px py cw ch)
                    (doseq [[i ^Line l] (map-indexed vector grid)]
                      (let [f (/ (inc (mod i 2)) 3.0)]
                        (if (< i 2)
                          (doto l (.setStartX (+ px (* cw f))) (.setEndX (+ px (* cw f))) (.setStartY py) (.setEndY (+ py ch)))
                          (doto l (.setStartY (+ py (* ch f))) (.setEndY (+ py (* ch f))) (.setStartX px) (.setEndX (+ px cw))))))
                    (doseq [[k ^Rectangle r] handles]
                      (let [hx (case k (:nw :w :sw) px (:n :s) (+ px (/ cw 2)) (+ px cw))
                            hy (case k (:nw :n :ne) py (:w :e) (+ py (/ ch 2)) (+ py ch))]
                        (.setX r (- hx (/ handle-px 2))) (.setY r (- hy (/ handle-px 2)))))))
        place!  (fn []
                  (let [^Bounds b (.getBoundsInParent iv)]
                    (.setLayoutX pane (.getMinX b)) (.setLayoutY pane (.getMinY b))
                    (.resize pane (.getWidth b) (.getHeight b))
                    (draw!)))
        apply-rect! (fn [r]
                      (reset! rect r)
                      (draw!)
                      (st/set-adj! :crop (mapv double r)))]
    (apply w/add! pane (concat dims [border] grid (vals handles)))
    (.addListener (.boundsInParentProperty iv) (w/change-listener (fn [_] (place!))))
    (.setOnMouseMoved pane
                      (w/handler
                        (fn [^MouseEvent e]
                          (let [[px py] (frac e) [pw ph] (size)
                                hit (crop/handle-at @rect px py (/ grab-px pw) (/ grab-px ph))]
                            (.setCursor pane (or (cursors hit) Cursor/CROSSHAIR))))))
    (.setOnMousePressed pane
                        (w/handler
                          (fn [^MouseEvent e]
                            (let [[px py] (frac e) [pw ph] (size)
                                  hit (crop/handle-at @rect px py (/ grab-px pw) (/ grab-px ph))]
                              ;; a crop that already fills the frame cannot move, so a drag
                              ;; inside it draws a new one
                              (reset! drag {:mode (if (or (nil? hit) (and (= hit :move) (crop/full? @rect))) :new hit)
                                            :rect @rect :start [px py]})))))
    (.setOnMouseDragged pane
                        (w/handler
                          (fn [^MouseEvent e]
                            (when-let [{:keys [mode start] r0 :rect} @drag]
                              (let [[px py] (frac e) [sx sy] start]
                                (apply-rect! (case mode
                                               :move (crop/move r0 (- px sx) (- py sy))
                                               :new  (crop/from-corners sx sy px py (lock))
                                               (crop/resize r0 mode px py (lock)))))))))
    (.setOnMouseReleased pane (w/handler (fn [_] (when @drag (reset! drag nil) (st/commit! "CROP")))))
    {:node pane
     :refresh!
     (fn [s]
       (let [on? (boolean (and (= :crop (:tab s)) (= :develop (:view s)) (:cur s) (not (:before s))))
             adj (when (:cur s) (st/cur-adj s))]
         (.setVisible pane on?) (.setMouseTransparent pane (not on?))
         (when (and on? adj (not @drag))
           (place!)                              ; sizes the pane to the image first
           (let [[pw ph] (size)]
             (reset! aspect (or (:aspect adj) "orig"))
             (reset! rect (geometry/crop-fractions adj pw ph))
             (draw!)))))}))
