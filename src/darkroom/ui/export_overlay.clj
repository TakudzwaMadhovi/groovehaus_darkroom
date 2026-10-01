(ns darkroom.ui.export-overlay
  "Export overlay: format, long edge, JPEG quality, destination folder. Renders
  the full-resolution original through the same pipeline as the preview."
  (:require [clojure.string :as str]
            [darkroom.catalog :as cat]
            [darkroom.imaging.color :as color]
            [darkroom.imaging.export :as export]
            [darkroom.imaging.output :as output]
            [darkroom.ui.fx :as fx]
            [darkroom.ui.state :as st]
            [darkroom.ui.theme :as theme]
            [darkroom.ui.widgets :as w])
  (:import (java.io File)
           (javafx.scene.control ScrollPane ScrollPane$ScrollBarPolicy TextField)
           (java.util.concurrent ExecutorService)
           (javafx.application Platform)
           (javafx.geometry Insets Pos)
           (javafx.scene.input KeyCode)
           (javafx.scene.layout FlowPane Pane Priority StackPane VBox)
           (javafx.stage DirectoryChooser Window)))

(set! *warn-on-reflection* false)

(defonce ^:private ^ExecutorService worker (fx/daemon-executor "darkroom-export" 1))

(defn target-name
  "First free `base.ext`, `base-2.ext`, ... in `dir`: never overwrites a file."
  [dir base fmt]
  (output/free-name dir base fmt))

(defn- start-dir [s]
  (or (:export-dir s)
      (some-> (:cur s) (File.) .getParentFile)
      (File. (System/getProperty "user.home"))))

(def export-presets
  "One-tap starting points: [label {state updates}]."
  [["WEB"    {:fmt :jpeg :size 2048 :q 85 :cspace :srgb :export-opts {:sharpen :screen :sharpen-amount :standard :metadata :copyright}}]
   ["PRINT"  {:fmt :tiff :size 0 :cspace :adobe-rgb :export-opts {:sharpen :glossy :sharpen-amount :standard :metadata :all}}]
   ["SOCIAL" {:fmt :webp :size 1080 :q 80 :cspace :srgb :export-opts {:sharpen :screen :sharpen-amount :low :metadata :none}}]])

(defn apply-export-preset
  "State `s` with the named preset's settings merged in (export-opts merged key by key)."
  [s label]
  (if-let [[_ u] (first (filter #(= label (first %)) export-presets))]
    (-> s (merge (dissoc u :export-opts)) (update :export-opts merge (:export-opts u)))
    s))

(defn export-options
  "The imaging/output options for state `s`."
  [s]
  (let [o (:export-opts s)]
    {:dir (start-dir s) :template (:template o) :format (:fmt s) :quality (/ (:q s) 100.0) :size (:size s) :space (:cspace s)
     :sharpen (when (:sharpen o) {:for (:sharpen o) :amount (:sharpen-amount o)})
     :metadata (:metadata o)
     :watermark (when-not (str/blank? (:wm-text o)) {:text (:wm-text o) :position (:wm-pos o) :opacity 0.6})}))

(defn frames-to-export
  "The frames for the chosen scope as imaging/output frames."
  [s]
  (let [c (:catalog s)
        paths (case (get-in s [:export-opts :scope])
                :selection (st/selection s)
                :shoot (st/frames s)
                (some-> (:cur s) vector))
        shoot (:name (cat/shoot c (:shoot s)))]
    (mapv (fn [p] {:path p :adj (cat/adj c p) :shoot shoot :rating (cat/rating c p) :meta (cat/frame-meta c p)}) paths)))

(defn run-export!
  "Renders and saves the chosen frames on the export thread. Calls
  (on-progress done total) after each frame, then (on-done {:files :failed}) or
  (on-error Throwable), all on the FX thread."
  [s on-progress on-done on-error]
  (let [frames (frames-to-export s) opts (export-options s)]
    (.execute worker
              (fn []
                (try
                  (let [r (output/export-frames! frames opts (fn [i n] (Platform/runLater #(on-progress i n))))]
                    (Platform/runLater #(on-done r)))
                  (catch Throwable t
                    (Platform/runLater #(on-error t))))))))

(defn- pill-group
  "Rebuilds `box` as a wrapping row of pills `items` ([value label a11y]), the one equal to `current` on."
  [^Pane box items current on-pick]
  (w/keep-focus!
    box
    (fn []
      (w/clear! box)
      (w/add! box
              (let [fp (FlowPane. 8.0 8.0)]
                (doseq [[v label a11y] items]
                  (let [b (w/pill label (fn [] (on-pick v)))]
                    (when a11y (w/set-base-a11y! b a11y))
                    (w/set-on! b (= v current))
                    (.add (.getChildren fp) b)))
                fp)))))

(defn- set-opt! [k v] (swap! st/state assoc-in [:export-opts k] v))

(defn- text-row
  "A TextField bound to export option `k` (committed on every keystroke)."
  [k prompt a11y]
  (let [f (doto (TextField.) (.setPromptText prompt))]
    (w/classes! f "gh-input")
    (w/a11y! f a11y)
    (.addListener (.textProperty f) (w/change-listener (fn [t] (set-opt! k t))))
    f))

(defn create
  "Returns {:node :refresh!}. The node fills its parent as a scrim."
  []
  (let [eyebrow  (w/tlabel "EXPORT" :wide "sys" "sun")
        title    (doto (w/label "" "display") (.setStyle "-fx-font-size: 56px;"))
        boxes    (into {} (map (fn [k] [k (w/vbox 8)])
                               [:preset :scope :fmt :size :space :sharpen :sharpen-amount :metadata :wm-pos]))
        box      (fn [k] (boxes k))
        q-row    (w/slider-row {:label "QUALITY" :min 50 :max 100 :step 1 :value 90 :default 90 :decimals 0
                                :on-input (fn [v] (swap! st/state assoc :q (long v)))})
        template (text-row :template "groovehaus_{name}" "File name template")
        wm-text  (text-row :wm-text "watermark text (optional)" "Watermark text")
        _        (.setText template (:template (:export-opts @st/state)))
        dir-lbl  (doto (w/label "" "editorial") (.setWrapText true) (.setStyle "-fx-font-size: 17px;"))
        name-lbl (doto (w/label "" "editorial") (.setWrapText true) (.setStyle "-fx-font-size: 17px;"))
        close    (fn [] (swap! st/state assoc :exporting false))
        busy     (atom false)
        go-btn   (w/pill "EXPORT ↓" nil "fill-sun")
        choose   (w/button (theme/tracked "CHOOSE →" :normal)
                           (fn []
                             (let [ch (doto (DirectoryChooser.) (.setTitle "Export to folder"))
                                   d  (start-dir @st/state)]
                               (when (.isDirectory d) (.setInitialDirectory ch d))
                               (when-let [f (.showDialog ch (some-> (.getScene go-btn) .getWindow))]
                                 (swap! st/state assoc :export-dir f))))
                           "text-btn" "short" "sun")
        section  (fn [title & nodes] (apply w/vbox 8 (w/tlabel title :normal "sys" "dim") nodes))
        card     (doto (VBox. 22.0) (.setMaxWidth 460) (.setMinWidth 300) (.setPrefWidth 460) (.setMaxHeight Double/NEGATIVE_INFINITY))
        holder   (doto (ScrollPane. card) (.setFitToWidth true) (.setHbarPolicy ScrollPane$ScrollBarPolicy/NEVER)
                   (.setMaxWidth 500) (.setMinWidth 320) (.setPrefWidth 500))
        scrim    (StackPane.)]
    (w/classes! card "export-card")
    (w/classes! scrim "scrim")
    (.setStyle holder "-fx-background-color: transparent; -fx-background: transparent;")
    (.setAlignment scrim Pos/CENTER)
    (.setPadding scrim (Insets. 16))
    (.setOnMouseClicked scrim (w/handler (fn [_] (close))))
    (.setOnMouseClicked holder (w/handler (fn [e] (.consume e))))
    (.setOnAction go-btn
                  (w/handler
                    (fn [_]
                      (when-not @busy
                        (reset! busy true)
                        (.setDisable go-btn true)
                        (let [finish! (fn [] (reset! busy false) (.setDisable go-btn false))]
                          (run-export! @st/state
                                       (fn [i n] (when (> n 1) (st/toast! (str "EXPORTING " i " / " n))))
                                       (fn [{:keys [files failed]}]
                                         (finish!) (close)
                                         (st/toast! (cond (and (seq files) (empty? failed))
                                                          (if (= 1 (count files)) (str "EXPORTED → " (.getName ^File (first files))) (str "EXPORTED " (count files) " FILES"))
                                                          (seq files) (str "EXPORTED " (count files) " · " (count failed) " FAILED")
                                                          :else (str "EXPORT FAILED — " (some-> failed first second)))))
                                       (fn [^Throwable t]
                                         (finish!)
                                         (st/toast! (str "EXPORT FAILED — " (.getMessage t))))))))))
    (w/add! card
            (w/vbox 8 eyebrow title)
            (section "PRESET" (box :preset))
            (section "EXPORT" (box :scope))
            (section "FORMAT" (box :fmt))
            (section "LONG EDGE" (box :size))
            (section "COLOUR SPACE" (box :space))
            (:node q-row)
            (section "OUTPUT SHARPENING" (box :sharpen) (box :sharpen-amount))
            (section "METADATA" (box :metadata))
            (section "WATERMARK" wm-text (box :wm-pos))
            (section "FILE NAME" template)
            (w/vbox 4 (w/hbox 8 (w/tlabel "FOLDER" :normal "sys" "dim") (w/spacer) choose) dir-lbl)
            name-lbl
            (doto (w/hbox 0 (w/button (theme/tracked "CANCEL" :normal) close "text-btn" "quiet") (w/spacer) go-btn)
              (.setPadding (Insets. 20 0 0 0))
              (.setStyle "-fx-border-color: rgba(242,233,213,0.14) transparent transparent transparent; -fx-border-width: 1 0 0 0;")))
    (w/add! scrim holder)
    (.setAccessibleText card "Export dialog")
    (.setAccessibleText choose "Choose export folder")
    {:node scrim
     :focus-first! (fn [] (when-let [b (first (.getChildren (first (.getChildren (box :fmt)))))] (.requestFocus b)))
     :refresh!
     (fn [s]
       (let [open? (boolean (:exporting s))]
         (.setVisible scrim open?) (.setManaged scrim open?)
         (when (and open? (:cur s))
           (let [o (:export-opts s) fmt (:fmt s) webp? (= :webp fmt)
                 n-sel (count (st/selection s)) n-all (count (st/frames s))]
             (.setText title (cat/frame-name (:cur s)))
             (pill-group (box :preset) (map (fn [[l _]] [l (theme/tracked l :normal) (str "Preset " l)]) export-presets) nil
                         (fn [l] (swap! st/state apply-export-preset l)))
             (pill-group (box :scope)
                         [[:frame "THIS FRAME" "Export this frame"]
                          [:selection (str "SELECTION " n-sel) (str "Export the " n-sel " selected frames")]
                          [:shoot (str "SHOOT " n-all) (str "Export all " n-all " frames of the shoot")]]
                         (:scope o) (fn [v] (set-opt! :scope v)))
             (pill-group (box :fmt) (map (fn [f] [f (.toUpperCase ^String (name f)) (:label (export/formats f))]) [:jpeg :png :tiff :tiff32 :webp])
                         fmt (fn [f] (swap! st/state assoc :fmt f)))
             (pill-group (box :size) [[1080 "1080 PX" "Long edge 1080 pixels"] [2048 "2048 PX" "Long edge 2048 pixels"] [0 "FULL RES" "Full resolution"]]
                         (:size s) (fn [v] (swap! st/state assoc :size v)))
             (if (= :tiff32 fmt)
               (pill-group (box :space) [[:none "LINEAR WORKING SPACE" "Scene-referred: stored unconverted in the linear working space"]] :none (fn [_] nil))
               (pill-group (box :space) (map (fn [sp] [sp (.toUpperCase ^String (:label (color/spaces sp))) (str "Colour space " (:label (color/spaces sp)))])
                                             [:srgb :display-p3 :adobe-rgb])
                           (if webp? :srgb (:cspace s)) (fn [sp] (when-not webp? (swap! st/state assoc :cspace sp)))))
             (pill-group (box :sharpen) [[nil "OFF" "No output sharpening"] [:screen "SCREEN" "Sharpen for screen"]
                                         [:matte "MATTE PRINT" "Sharpen for matte paper"] [:glossy "GLOSSY PRINT" "Sharpen for glossy paper"]]
                         (:sharpen o) (fn [v] (set-opt! :sharpen v)))
             (pill-group (box :sharpen-amount) [[:low "LOW" "Low sharpening"] [:standard "STANDARD" "Standard sharpening"] [:high "HIGH" "High sharpening"]]
                         (:sharpen-amount o) (fn [v] (set-opt! :sharpen-amount v)))
             (.setVisible (box :sharpen-amount) (boolean (:sharpen o))) (.setManaged (box :sharpen-amount) (boolean (:sharpen o)))
             (pill-group (box :metadata) [[:all "ALL" "Write camera data, creator and copyright"]
                                          [:copyright "COPYRIGHT ONLY" "Write only creator and copyright"]
                                          [:none "NONE" "Write no metadata"]]
                         (:metadata o) (fn [v] (set-opt! :metadata v)))
             (pill-group (box :wm-pos) (map (fn [p] [p (.toUpperCase (str/replace (name p) "-" " ")) (str "Watermark position " (str/replace (name p) "-" " "))])
                                            output/watermark-positions)
                         (:wm-pos o) (fn [v] (set-opt! :wm-pos v)))
             (let [^Pane qn (:node q-row) lossy? (boolean (#{:jpeg :webp} fmt))]
               (.setVisible qn lossy?) (.setManaged qn lossy?))
             (let [d (start-dir s)
                   tpl (output/expand-template (:template o) {:name (str/lower-case (cat/frame-name (:cur s))) :n 1 :date (output/capture-date {})
                                                              :rating (cat/rating (:catalog s) (:cur s)) :shoot (:name (cat/shoot (:catalog s) (:shoot s)))})
                   cnt (count (frames-to-export s))]
               (.setText dir-lbl (str d))
               (.setText name-lbl (str (when (> cnt 1) (str cnt " files · ")) (target-name d tpl fmt) "." (:ext (export/formats fmt)))))))))}))
