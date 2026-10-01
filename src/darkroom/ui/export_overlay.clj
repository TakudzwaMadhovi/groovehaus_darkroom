(ns darkroom.ui.export-overlay
  "Export overlay: format, long edge, JPEG quality, destination folder. Renders
  the full-resolution original through the same pipeline as the preview."
  (:require [darkroom.catalog :as cat]
            [darkroom.imaging.color :as color]
            [darkroom.imaging.develop :as develop]
            [darkroom.imaging.exif :as exif]
            [darkroom.imaging.export :as export]
            [darkroom.imaging.loader :as loader]
            [darkroom.imaging.pipeline :as pipeline]
            [darkroom.ui.fx :as fx]
            [darkroom.ui.state :as st]
            [darkroom.ui.theme :as theme]
            [darkroom.ui.widgets :as w])
  (:import (java.io File)
           (java.util.concurrent ExecutorService)
           (javafx.application Platform)
           (javafx.geometry Insets Pos)
           (javafx.scene.input KeyCode)
           (javafx.scene.layout FlowPane Pane Priority StackPane VBox)
           (javafx.stage DirectoryChooser Window)))

(set! *warn-on-reflection* false)

(defonce ^:private ^ExecutorService worker (fx/daemon-executor "darkroom-export" 1))

(defn- base-name [path] (str "groovehaus_" (.toLowerCase (cat/frame-name path))))

(defn target-name
  "First free `base.ext`, `base-2.ext`, ... in `dir`: never overwrites a file."
  [dir base fmt]
  (loop [n 1]
    (let [name (if (= n 1) base (str base "-" n))]
      (if (.exists (export/target-file dir name fmt))
        (recur (inc n))
        name))))

(defn- start-dir [s]
  (or (:export-dir s)
      (some-> (:cur s) (File.) .getParentFile)
      (File. (System/getProperty "user.home"))))

(defn run-export!
  "Renders and saves the current frame on the export thread. Calls
  (on-done File) or (on-error Throwable) on the FX thread."
  [s on-done on-error]
  (let [path (:cur s)
        adj  (cat/adj (:catalog s) path)
        dir  (start-dir s)
        fmt  (:fmt s) size (:size s) q (:q s) space (:cspace s)]
    (.execute worker
              (fn []
                (try
                  (let [full (loader/load-scene path)
                        out  (-> (pipeline/render full adj {:quality :final})
                                 (develop/resize-long-edge (when (pos? size) size)))
                        name (target-name dir (base-name path) fmt)
                        f    (export/save-scene! out {:dir dir :name name :format fmt :quality (/ q 100.0)
                                                      :space space :tags (exif/read-tags path)})]
                    (Platform/runLater #(on-done f)))
                  (catch Throwable t
                    (Platform/runLater #(on-error t))))))))

(defn create
  "Returns {:node :refresh!}. The node fills its parent as a scrim."
  []
  (let [eyebrow  (w/tlabel "EXPORT" :wide "sys" "sun")
        title    (doto (w/label "" "display") (.setStyle "-fx-font-size: 56px;"))
        fmt-box  (w/vbox 8)
        size-box (w/vbox 8)
        space-box (w/vbox 8)
        q-row    (w/slider-row {:label "QUALITY" :min 50 :max 100 :step 1 :value 90 :default 90 :decimals 0
                                :on-input (fn [v] (swap! st/state assoc :q (long v)))})
        dir-lbl  (doto (w/label "" "editorial") (.setWrapText true) (.setStyle "-fx-font-size: 17px;"))
        name-lbl (doto (w/label "" "editorial") (.setStyle "-fx-font-size: 17px;"))
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
        card     (doto (VBox. 22.0) (.setMaxWidth 460) (.setMinWidth 300) (.setPrefWidth 460) (.setMaxHeight Double/NEGATIVE_INFINITY))
        scrim    (StackPane.)]
    (w/classes! card "export-card")
    (w/classes! scrim "scrim")
    (.setAlignment scrim Pos/CENTER)
    (.setPadding scrim (Insets. 16))
    (.setOnMouseClicked scrim (w/handler (fn [_] (close))))
    (.setOnMouseClicked card (w/handler (fn [e] (.consume e))))
    (.setOnAction go-btn
                  (w/handler
                    (fn [_]
                      (when-not @busy
                        (reset! busy true)
                        (.setDisable go-btn true)
                        (run-export! @st/state
                                     (fn [^File f]
                                       (reset! busy false) (.setDisable go-btn false) (close)
                                       (st/toast! (str "EXPORTED → " (.getName f))))
                                     (fn [^Throwable t]
                                       (reset! busy false) (.setDisable go-btn false)
                                       (st/toast! (str "EXPORT FAILED — " (.getMessage t)))))))))
    (w/add! card
            (w/vbox 8 eyebrow title)
            (w/vbox 8 (w/tlabel "FORMAT" :normal "sys" "dim") fmt-box)
            (w/vbox 8 (w/tlabel "LONG EDGE" :normal "sys" "dim") size-box)
            (w/vbox 8 (w/tlabel "COLOUR SPACE" :normal "sys" "dim") space-box)
            (:node q-row)
            (w/vbox 4 (w/hbox 8 (w/tlabel "FOLDER" :normal "sys" "dim") (w/spacer) choose) dir-lbl)
            name-lbl
            (doto (w/hbox 0 (w/button (theme/tracked "CANCEL" :normal) close "text-btn" "quiet") (w/spacer) go-btn)
              (.setPadding (Insets. 20 0 0 0))
              (.setStyle "-fx-border-color: rgba(242,233,213,0.14) transparent transparent transparent; -fx-border-width: 1 0 0 0;")))
    (w/add! scrim card)
    (.setAccessibleText card "Export dialog")
    (.setAccessibleText choose "Choose export folder")
    {:node scrim
     :focus-first! (fn [] (when-let [b (first (.getChildren (first (.getChildren fmt-box))))] (.requestFocus b)))
     :refresh!
     (fn [s]
       (let [open? (boolean (:exporting s))]
         (.setVisible scrim open?) (.setManaged scrim open?)
         (when (and open? (:cur s))
           (.setText title (cat/frame-name (:cur s)))
           (w/keep-focus! fmt-box
                          (fn []
                            (w/clear! fmt-box)
                            (w/add! fmt-box
                                    (let [fp (FlowPane. 8.0 8.0)]
                                      (doseq [f [:jpeg :png]]
                                        (let [b (w/pill (.toUpperCase (name f)) (fn [] (swap! st/state assoc :fmt f)))]
                                          (w/set-on! b (= f (:fmt s)))
                                          (.add (.getChildren fp) b)))
                                      fp))))
           (w/keep-focus! size-box
                          (fn []
                            (w/clear! size-box)
                            (w/add! size-box
                                    (let [fp (FlowPane. 8.0 8.0)]
                                      (doseq [[v l] [[1080 "1080 PX"] [2048 "2048 PX"] [0 "FULL RES"]]]
                                        (let [b (w/pill l (fn [] (swap! st/state assoc :size v)))]
                                          (w/set-base-a11y! b (if (zero? v) "Full resolution" (str "Long edge " v " pixels")))
                                          (w/set-on! b (= v (:size s)))
                                          (.add (.getChildren fp) b)))
                                      fp))))
           (w/keep-focus! space-box
                          (fn []
                            (w/clear! space-box)
                            (w/add! space-box
                                    (let [fp (FlowPane. 8.0 8.0)]
                                      (doseq [sp [:srgb :display-p3 :adobe-rgb]]
                                        (let [label (:label (color/spaces sp))
                                              b (w/pill (.toUpperCase ^String label) (fn [] (swap! st/state assoc :cspace sp)))]
                                          (w/set-base-a11y! b (str "Colour space " label))
                                          (w/set-on! b (= sp (:cspace s)))
                                          (.add (.getChildren fp) b)))
                                      fp))))
           (let [^Pane qn (:node q-row)]
             (.setVisible qn (= :jpeg (:fmt s))) (.setManaged qn (= :jpeg (:fmt s))))
           (let [d (start-dir s)]
             (.setText dir-lbl (str d))
             (.setText name-lbl (str (target-name d (base-name (:cur s)) (:fmt s))
                                     (if (= :png (:fmt s)) ".png" ".jpg")))))))}))
