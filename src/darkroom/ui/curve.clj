(ns darkroom.ui.curve
  "Tone-curve editor: 240x240 plot, 5 draggable points at x = 0, 1/4, 1/2, 3/4, 1,
  identity diagonal dashed, curve drawn through the same spline the engine uses.

  Keyboard: Tab focuses the plot; Left/Right choose a point; Up/Down move it by
  1% (Shift 5%, Page Up/Down 10%); Home/End jump to 0%/100%; Backspace resets the
  point. The focused point gets a halo and is announced with its value."
  (:require [darkroom.imaging.develop :as develop]
            [darkroom.ui.state :as st]
            [darkroom.ui.theme :as theme]
            [darkroom.ui.widgets :as w])
  (:import (javafx.scene AccessibleRole)
           (javafx.scene.input KeyCode KeyEvent MouseEvent)
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
        active   (atom 2)                       ; point chosen with the keyboard
        halo     (doto (Circle. 9.0) (.setFill nil) (.setStroke (theme/bone-a 0.9)) (.setStrokeWidth 1.5) (.setMouseTransparent true))
        announce! (fn [cv]
                    (let [i @active]
                      (.setAccessibleText pane (format "Tone curve point %d of 5, input %d percent, output %d percent"
                                                       (inc i) (* 25 i) (Math/round (* 100.0 (double (nth cv i))))))))
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
                       (.setCenterY (+ off (* (- 1.0 (double (nth cv i))) size)))))
                   (.setCenterX halo (+ off (* @active 60.0)))
                   (.setCenterY halo (+ off (* (- 1.0 (double (nth cv @active))) size)))
                   (.setVisible halo (.isFocused pane))
                   (announce! cv)))
        at     (fn [^MouseEvent e]
                 (let [x (/ (- (.getX e) off) size)
                       y (- 1.0 (/ (- (.getY e) off) size))
                       i (max 0 (min 4 (Math/round (* x 4.0))))
                       cv (vec (get (st/cur-adj @st/state) :curve develop/default-curve))]
                   (reset! active i)
                   (st/set-adj! :curve (assoc cv i (/ (Math/round (* 100.0 (clamp01 y))) 100.0)))))]
    (doseq [q [0.25 0.5 0.75]]
      (.setStyle grid "")
      (doto (.getElements grid)
        (.add (MoveTo. (+ off (* q size)) off)) (.add (LineTo. (+ off (* q size)) (+ off size)))
        (.add (MoveTo. off (+ off (* q size)))) (.add (LineTo. (+ off size) (+ off (* q size))))))
    (.addAll (.getStrokeDashArray diag) (java.util.Arrays/asList (into-array Double [3.0 4.0])))
    (apply w/add! pane frame grid diag curve (concat pts [halo]))
    (.setStyle pane "-fx-cursor: crosshair;")
    (let [cur-curve #(vec (get (st/cur-adj @st/state) :curve develop/default-curve))
          move!     (fn [delta]
                      (let [cv (cur-curve) i @active]
                        (st/set-adj! :curve (assoc cv i (/ (Math/round (* 100.0 (clamp01 (+ (double (nth cv i)) delta)))) 100.0)))))
          set-pt!   (fn [y] (st/set-adj! :curve (assoc (cur-curve) @active y)))]
      (.setFocusTraversable pane true)
      (.put (.getProperties pane) "owns-arrows" true)
      (.setAccessibleRole pane AccessibleRole/SLIDER)
      (.setAccessibleHelp pane "Left and Right choose a point. Up and Down move it; hold Shift for larger steps. Backspace resets the point.")
      (w/classes! pane "curve-pane")
      (.addListener (.focusedProperty pane)
                    (w/change-listener (fn [f] (.setVisible halo (boolean f)) (sync! (st/cur-adj @st/state)))))
      (.setOnKeyPressed
        pane
        (w/handler
          (fn [^KeyEvent e]
            (let [code (.getCode e) step (if (.isShiftDown e) 0.05 0.01)]
              (condp = code
                KeyCode/LEFT      (do (swap! active #(max 0 (dec %))) (sync! (st/cur-adj @st/state)) (.consume e))
                KeyCode/RIGHT     (do (swap! active #(min 4 (inc %))) (sync! (st/cur-adj @st/state)) (.consume e))
                KeyCode/UP        (do (move! step) (.consume e))
                KeyCode/DOWN      (do (move! (- step)) (.consume e))
                KeyCode/PAGE_UP   (do (move! 0.1) (.consume e))
                KeyCode/PAGE_DOWN (do (move! -0.1) (.consume e))
                KeyCode/HOME      (do (set-pt! 0.0) (.consume e))
                KeyCode/END       (do (set-pt! 1.0) (.consume e))
                KeyCode/BACK_SPACE (do (set-pt! (nth develop/default-curve @active)) (.consume e))
                nil)))))
      (.setOnKeyReleased pane (w/handler (fn [^KeyEvent e]
                                           (when (#{KeyCode/UP KeyCode/DOWN KeyCode/PAGE_UP KeyCode/PAGE_DOWN KeyCode/HOME KeyCode/END KeyCode/BACK_SPACE}
                                                   (.getCode e))
                                             (st/commit! "TONE CURVE"))))))
    (.setOnMousePressed pane (w/handler (fn [e] (.requestFocus pane) (reset! dragging true) (at e))))
    (.setOnMouseDragged pane (w/handler (fn [e] (when @dragging (at e)))))
    (.setOnMouseReleased pane (w/handler (fn [_] (when @dragging (reset! dragging false) (st/commit! "TONE CURVE")))))
    (sync! {})
    {:node pane :sync! sync!}))
