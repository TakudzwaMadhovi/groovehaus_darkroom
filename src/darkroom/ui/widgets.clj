(ns darkroom.ui.widgets
  "Small JavaFX builders shared by the views: buttons, pills, labels and the
  slider row. All styling is in darkroom.css."
  (:require [darkroom.ui.theme :as theme])
  (:import (javafx.application Platform)
           (javafx.scene Parent)
           (javafx.scene.control Labeled)
           (javafx.beans.value ChangeListener)
           (javafx.event EventHandler)
           (javafx.geometry Pos)
           (javafx.scene Node)
           (javafx.scene.control Button Label Slider)
           (javafx.scene.input MouseEvent)
           (javafx.scene.layout HBox Pane Priority Region StackPane VBox)))

;; JavaFX wiring code: not performance-critical, so reflective interop is fine here.
(set! *warn-on-reflection* false)

(defn handler ^EventHandler [f] (reify EventHandler (handle [_ e] (f e))))

(defn change-listener ^ChangeListener [f] (reify ChangeListener (changed [_ _ _ v] (f v))))

(defn classes!
  "Adds style classes to a node; returns it."
  [^Node n & cs]
  (doseq [c cs :when c] (.add (.getStyleClass n) c))
  n)

(defn add!
  "Adds `nodes` (nil ignored) to a Pane's children; returns the pane."
  [^Pane p & nodes]
  (doseq [n nodes :when n] (.add (.getChildren p) ^Node n))
  p)

(defn clear! [^Pane p] (.clear (.getChildren p)) p)

(defn hbox [spacing & nodes] (apply add! (doto (HBox. (double spacing)) (.setAlignment Pos/CENTER_LEFT)) nodes))
(defn vbox [spacing & nodes] (apply add! (VBox. (double spacing)) nodes))

(defn grow! [^Node n] (HBox/setHgrow n Priority/ALWAYS) (VBox/setVgrow n Priority/ALWAYS) n)

(defn spacer [] (grow! (Region.)))

(defn a11y!
  "Sets the text a screen reader announces for `n`; returns `n`."
  [^Node n text]
  (.setAccessibleText n (str text))
  n)

(defn- track-a11y!
  "Keeps a Labeled's accessible text in sync with its visible text, minus the
  letter-spacing characters used for tracked labels."
  [^Labeled n]
  (let [sync! (fn []
                (let [t (.getText n)]
                  (when (and t (not= t (theme/untracked t)))
                    (.setAccessibleText n (theme/untracked t)))))]
    (sync!)
    (.addListener (.textProperty n) (change-listener (fn [_] (sync!))))
    n))

(defn label
  "Label with style classes."
  ^Label [text & cs]
  (track-a11y! (apply classes! (Label. (str text)) cs)))

(defn tlabel
  "Letter-spaced label (level :tight / :normal / :wide); announced untracked."
  [text level & cs]
  (apply label (theme/tracked text level) cs))

(defn button
  "Flat button. `on-action` is called with no args."
  ^Button [text on-action & cs]
  (let [b (Button. (str text))]
    (apply classes! b "gh-btn" cs)
    (when on-action (.setOnAction b (handler (fn [_] (on-action)))))
    (track-a11y! b)))

(defn pill [text on-action & cs]
  (apply button (theme/tracked text :normal) on-action "pill" cs))

(defn set-classes!
  "Sets/clears one class on a node."
  [^Node n c on?]
  (if on? (when-not (.contains (.getStyleClass n) c) (.add (.getStyleClass n) c))
          (.remove (.getStyleClass n) c)))

(defn set-on!
  "Marks a toggle-like control (tab, pill, row) as on/off: toggles the `on`
  style class and announces ', selected' after its name."
  [^Node n on?]
  (set-classes! n "on" (boolean on?))
  (let [base (or (some-> n .getProperties (.get "a11y-base"))
                 (let [t (or (.getAccessibleText n) (when (instance? Labeled n) (theme/untracked (.getText ^Labeled n))))]
                   (.put (.getProperties n) "a11y-base" t)
                   t))]
    (when (seq base)
      (.setAccessibleText n (str base (when on? ", selected")))))
  n)

(defn set-base-a11y!
  "Sets the accessible name used by `set-on!` (when the visible text isn't it)."
  [^Node n text]
  (.put (.getProperties n) "a11y-base" (str text))
  (.setAccessibleText n (str text))
  n)

(defn focus-path
  "Child-index path from `container` down to `owner`, or nil if `owner` is not
  inside it."
  [^Parent container ^Node owner]
  (loop [n owner path ()]
    (cond (nil? n) nil
          (identical? n container) (vec path)
          :else (let [p (.getParent n)]
                  (when p (recur p (cons (.indexOf (.getChildrenUnmodifiable p) n) path)))))))

(defn keep-focus!
  "Calls `rebuild` (which replaces `container`'s children) and, if keyboard focus
  was inside the container, puts it back on the node at the same position, so
  rebuilding a list never drops a keyboard user's place. `target` (optional)
  is a fn of the new container returning the node to focus instead."
  ([^Parent container rebuild] (keep-focus! container rebuild nil))
  ([^Parent container rebuild target]
   (let [owner (some-> container .getScene .getFocusOwner)
         path  (when owner (focus-path container owner))]
     (rebuild)
     (when path
       (Platform/runLater
         (fn []
           (let [^Node n (or (when target (target container))
                             (loop [cur container [i & more :as p] path]
                               (if (or (empty? p) (not (instance? Parent cur)))
                                 cur
                                 (let [kids (.getChildrenUnmodifiable ^Parent cur)]
                                   (if (empty? kids) cur (recur (.get kids (min i (dec (count kids)))) more))))))]
             (.requestFocus n))))))))

(defn click-count [^MouseEvent e] (.getClickCount e))

;; ------------------------------------------------------------------ slider

(defn- fmt-value [{:keys [min decimals pct?]} v]
  (let [v (double v)]
    (str (if (and (> v 0) (neg? (double min))) "+" "")
         (if pct?
           (Math/round (* v 100.0))
           (format (str "%." (or decimals 2) "f") v)))))

(defn slider-row
  "Label + live value readout + restyled slider. Spec keys: :label :min :max
  :step :value :default :decimals :pct? and callbacks :on-input (v, live),
  :on-commit (on release), :on-reset. Returns {:node :set-value! :slider}.
  `set-value!` updates the control without firing callbacks."
  [{:keys [label step default value on-input on-commit on-reset] lo :min hi :max :as spec}]
  (let [slider   (doto (Slider. (double lo) (double hi) (double value))
                   (.setBlockIncrement (double step))
                   (.setMajorTickUnit (double (- hi lo))))
        center?  (neg? (double lo))
        suppress (atom false)
        ^Label shown (classes! (Label.) "display")
        name-btn (button (theme/tracked label :wide) (fn [] (when on-reset (on-reset)))
                         "quiet-label")
        round-to (fn [v] (let [s (double step)] (* s (Math/round (/ (double v) s)))))
        paint!   (fn []
                   (when-let [^Node track (.lookup slider ".track")]
                     (let [w   (max 1.0 (.getWidth slider))
                           p   (/ (- (.getValue slider) lo) (double (- hi lo)))
                           
                           from (if center? 0.5 0.0)
                           lo  (Math/min p from) hi (Math/max p from)
                           stop (fn [q] (format "%.2f%%" (* 100.0 (/ (+ 7.0 (* (- w 14.0) q)) w))))
                           t   "rgba(242,233,213,0.2)" f "#F2E9D5"]
                       (.setStyle track
                                  (format "-fx-background-color: linear-gradient(to right, %s 0%%, %s %s, %s %s, %s %s, %s %s, %s 100%%); -fx-background-insets: 13 0 13 0;"
                                          t t (stop lo) f (stop lo) f (stop hi) t (stop hi) t)))))
        refresh! (fn [v]
                   (let [v (double v)]
                     (.setText shown (fmt-value spec v))
                     (set-classes! shown "sun" (not= (double default) v))
                     (set-classes! shown "bone" (= (double default) v))
                     (paint!)))]
    (classes! slider "gh-slider")
    (.setAccessibleText slider (str label))
    (.setAccessibleHelp slider "Arrow keys adjust the value. Activate the reset button beside it to restore the default.")
    (a11y! name-btn (str "Reset " label))
    (.addListener (.valueProperty slider)
                  (change-listener
                    (fn [v]
                      (let [v (round-to (.doubleValue ^Number v))]
                        (refresh! v)
                        (when-not @suppress (when on-input (on-input v)))))))
    (.addListener (.widthProperty slider) (change-listener (fn [_] (Platform/runLater paint!))))
    (.addListener (.sceneProperty slider) (change-listener (fn [_] (Platform/runLater paint!))))
    (.setOnMouseReleased slider (handler (fn [_] (when (and on-commit (not @suppress)) (on-commit)))))
    (.setOnKeyReleased slider (handler (fn [_] (when on-commit (on-commit)))))
    (.setOnMouseClicked slider (handler (fn [e] (when (and (= 2 (click-count e)) on-reset) (on-reset)))))
    (refresh! value)
    (let [tick (doto (Region.) (.setMinSize 1 14) (.setPrefSize 1 14) (.setMaxSize 1 14)
                 (.setMouseTransparent true)
                 (.setStyle "-fx-background-color: rgba(242,233,213,0.35);"))
          stack (doto (StackPane.) (.setAlignment Pos/CENTER))
          head  (doto (HBox.) (.setAlignment Pos/BASELINE_LEFT))]
      (add! head name-btn (spacer) shown)
      (add! stack slider (when center? tick))
      {:node (vbox 2 head stack)
       :slider slider
       :set-value! (fn [v]
                     (reset! suppress true)
                     (try (.setValue slider (double v)) (refresh! v)
                          (finally (reset! suppress false))))})))
