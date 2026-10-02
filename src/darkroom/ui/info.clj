(ns darkroom.ui.info
  "The info panel: camera details of the current frame, colour label and reject
  flag, title / caption / creator / copyright and keywords. Edits apply to the
  whole selection (see state/selection). Shown in the Library sidebar and as the
  Develop INFO tab."
  (:require [darkroom.catalog :as cat]
            [darkroom.imaging.exif :as exif]
            [darkroom.ui.state :as st]
            [darkroom.ui.theme :as theme]
            [darkroom.ui.widgets :as w])
  (:import (javafx.scene.control Button Label TextField)
           (javafx.scene.input KeyCode KeyEvent)
           (javafx.scene.layout FlowPane VBox)
           (javafx.scene.shape Circle)))

(set! *warn-on-reflection* false)

(def ^:private fields
  [[:title "TITLE"] [:caption "CAPTION"] [:creator "CREATOR"] [:copyright "COPYRIGHT"]])

(defn- swatch [colour]
  (let [b (Button.)]
    (w/classes! b "gh-btn" "swatch")
    (.setGraphic b (doto (Circle. 8.0) (.setFill (javafx.scene.paint.Color/web (theme/label-colours colour)))))
    (w/set-base-a11y! b (str (name colour) " label"))
    (.setOnAction b (w/handler (fn [_] (st/label! colour))))
    b))

(defn- text-field
  "TextField that commits `on-commit` (with its text) on Enter and when it loses
  focus; with `clear?` the text is emptied after each commit."
  [prompt a11y on-commit clear?]
  (let [f (doto (TextField.) (.setPromptText prompt))
        commit! (fn [] (on-commit (.getText f)) (when clear? (.clear f)))]
    (w/classes! f "gh-input")
    (w/a11y! f a11y)
    (.addListener (.focusedProperty f) (w/change-listener (fn [focused] (when-not focused (commit!)))))
    (.setOnKeyPressed f (w/handler (fn [^KeyEvent e]
                                     (when (= (.getCode e) KeyCode/ENTER) (commit!) (.consume e)))))
    f))

(defn create
  "Returns {:node :sync! (fn [state])}."
  []
  (let [camera   (w/tlabel "" :normal "sys-11s" "muted")
        exposure (w/tlabel "" :normal "sys-11s" "muted")
        taken    (w/tlabel "" :normal "sys-10" "faint")
        scope    (w/tlabel "" :normal "sys-10" "sun")
        swatches (into {} (map (fn [c] [c (swatch c)]) cat/colour-labels))
        reject   (doto (w/pill "REJECT" (fn [] (st/reject!)) "xs") (w/set-base-a11y! "Reject (X)"))
        inputs   (into {} (for [[k l] fields]
                            [k (text-field (.toLowerCase ^String l) l (fn [t] (st/set-meta-if-changed! k t)) false)]))
        chips    (doto (FlowPane. 6.0 6.0))
        kw-input (text-field "add keywords, comma separated" "Add keywords"
                             (fn [t] (when-not (clojure.string/blank? t) (st/add-keywords! t))) true)
        memo     (atom nil)
        node     (VBox. 14.0)]
    (w/add! node
            (w/vbox 4 camera exposure taken)
            (w/vbox 6 (w/tlabel "LABEL" :wide "sys" "dim")
                    (let [fp (FlowPane. 6.0 6.0)]
                      (apply w/add! fp (concat (map swatches cat/colour-labels) [reject]))))
            (apply w/vbox 12
                   (for [[k l] fields] (w/vbox 4 (w/tlabel l :wide "sys" "dim") (inputs k))))
            (w/vbox 6 (w/tlabel "KEYWORDS" :wide "sys" "dim") chips kw-input)
            scope)
    {:node node
     :sync!
     (fn [s]
       (let [c (:catalog s) path (:cur s)
             ps (st/selection s)
             sig [path (:sel s) (:meta-rev s) (when path [(cat/colour c path) (cat/rejected? c path) (cat/frame-meta c path) (cat/keywords c path)])]]
         (when (not= sig @memo)
           (reset! memo sig)
           (.setDisable node (nil? path))
           (let [lines (concat (exif/summary (or (some-> path st/meta-of) {})) (repeat nil))]
             (doseq [[^Label l text] (map vector [camera exposure taken] lines)]
               (.setText l (or text ""))
               (.setManaged l (boolean text)) (.setVisible l (boolean text))))
           (.setText scope (theme/tracked (if (> (count ps) 1) (str "EDITS APPLY TO ALL " (count ps) " SELECTED FRAMES") "") :normal))
           (doseq [[col ^Button b] swatches]
             (w/set-on! b (boolean (and path (= col (cat/colour c path))))))
           (w/set-on! reject (boolean (and path (cat/rejected? c path))))
           (doseq [[k ^TextField f] inputs]
             (when-not (.isFocused f) (.setText f (str (when path (get (cat/frame-meta c path) k))))))
           (w/clear! chips)
           (when path
             (doseq [kw (cat/keywords c path)]
               (let [b (w/pill (str kw " ×") (fn [] (st/remove-keyword! kw)) "xs")]
                 (w/set-base-a11y! b (str "Remove keyword " kw))
                 (w/add! chips b)))))))}))
