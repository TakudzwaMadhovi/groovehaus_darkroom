(ns darkroom.ui.library
  "Library view: shoots sidebar, toolbar (filters, size), thumbnail grid and
  status bar. Reads state, calls actions; owns no data."
  (:require [clojure.string :as str]
            [darkroom.catalog :as cat]
            [darkroom.ui.state :as st]
            [darkroom.ui.theme :as theme]
            [darkroom.ui.thumbs :as thumbs]
            [darkroom.ui.widgets :as w])
  (:import (java.io File)
           (javafx.application Platform)
           (javafx.geometry Insets Pos Rectangle2D)
           (javafx.scene Node)
           (javafx.scene.control Button Label ScrollPane ScrollPane$ScrollBarPolicy TextField)
           (javafx.scene.image Image ImageView)
           (javafx.scene.input KeyCode KeyEvent MouseButton TransferMode)
           (javafx.scene.layout FlowPane HBox Pane Priority Region StackPane VBox)))

;; JavaFX wiring code: not performance-critical, so reflective interop is fine here.
(set! *warn-on-reflection* false)

(defn- dots [n] (str (apply str (repeat n "●")) (apply str (repeat (- 5 n) "○"))))

(defn- cover-image
  "Square, centre-cropped ImageView that fills a w x w box."
  [^double size]
  (doto (ImageView.) (.setFitWidth size) (.setFitHeight size) (.setSmooth true)))

(defn- crop-square! [^ImageView iv ^Image img]
  (let [w (.getWidth img) h (.getHeight img) m (min w h)]
    (.setViewport iv (Rectangle2D. (/ (- w m) 2.0) (/ (- h m) 2.0) m m))
    (.setImage iv img)))

;; ----------------------------------------------------------------- sidebar

(defn- shoot-row [{:keys [id name paths]} on?]
  (let [cover (doto (StackPane.) (.setMinSize 44 44) (.setPrefSize 44 44) (.setMaxSize 44 44))
        iv    (cover-image 44.0)
        _     (w/classes! cover "cover")
        label (w/tlabel name :tight "sys" "sys-12" (if on? "bone" "muted"))
        count (w/tlabel (str (count paths) (if (= 1 (count paths)) " FRAME" " FRAMES")) :normal "sys-10" "faint")
        text  (w/vbox 2 label count)
        row   (doto (Button.) (.setMaxWidth Double/MAX_VALUE))]
    (.setClip cover (doto (javafx.scene.shape.Rectangle. 44 44) (.setArcWidth 8) (.setArcHeight 8)))
    (w/add! cover iv)
    (.setMaxWidth label 150.0)
    (.setTextOverrun label javafx.scene.control.OverrunStyle/ELLIPSIS)
    (when-let [p (first paths)]
      (thumbs/request! p (constantly true) #(crop-square! iv %)))
    (w/classes! row "shoot-row")
    (w/set-classes! row "on" on?)
    (.setGraphic row (w/hbox 12 cover text))
    (.setText row "")
    (.setContentDisplay row javafx.scene.control.ContentDisplay/GRAPHIC_ONLY)
    (.setAlignment row Pos/CENTER_LEFT)
    (.setOnAction row (w/handler (fn [_] (st/select-shoot! id))))
    row))

(defn- add-form []
  (let [input  (doto (TextField.) (.setPromptText "name this shoot"))
        create (fn [] (st/create-shoot! (.getText input) []) (.clear input))
        cancel (fn [] (.clear input) (swap! st/state assoc :adding false))
        box    (doto (w/vbox 8
                              input
                              (w/hbox 16
                                      (w/button (theme/tracked "CREATE ↵" :normal) create "text-btn" "sun" "short")
                                      (w/button (theme/tracked "CANCEL" :normal) cancel "text-btn" "quiet" "short")))
                 (.setPadding (Insets. 0 20 12 20)))]
    (w/classes! input "gh-input")
    (.setOnKeyPressed input (w/handler (fn [^KeyEvent e]
                                         (.consume e)
                                         (condp = (.getCode e)
                                           KeyCode/ENTER (create)
                                           KeyCode/ESCAPE (cancel)
                                           nil))))
    {:node box :input input}))

;; -------------------------------------------------------------------- tiles

(defn- tile [path idx on? edited? rating size]
  (let [area  (doto (StackPane.) (.setPadding (Insets. 22 10 4 10)) (.setMinSize 0 0) (.setPrefSize 0 0))
        iv    (doto (ImageView.) (.setPreserveRatio true) (.setSmooth true))
        name  (w/tlabel (cat/frame-name path) :tight "sys" "sys-10")
        meta  (w/tlabel (str (when edited? "EDITED · ") (dots rating)) :tight "sys-10" "sys" "dim")
        bar   (doto (HBox. 8.0) (.setAlignment Pos/CENTER_LEFT) (.setPadding (Insets. 6 10 8 10)))
        body  (VBox.)
        num   (w/tlabel (format "%02d" idx) :tight "sys-10" "sys" "dim")
        star  (w/button (if (= 5 rating) "★" "☆") nil "star-btn")
        tile  (StackPane.)]
    (.bind (.fitWidthProperty iv) (.subtract (.widthProperty area) 20))
    (.bind (.fitHeightProperty iv) (.subtract (.heightProperty area) 26))
    (.setMaxWidth name 120.0)
    (w/classes! name "bone")
    (w/classes! bar "tile-bar")
    (w/set-classes! bar "on" on?)
    (w/add! area iv)
    (VBox/setVgrow area Priority/ALWAYS)
    (w/add! bar name (w/spacer) meta)
    (w/add! body area bar)
    (StackPane/setAlignment num Pos/TOP_LEFT)
    (StackPane/setMargin num (Insets. 6 0 0 8))
    (.setMouseTransparent num true)
    (StackPane/setAlignment star Pos/TOP_RIGHT)
    (w/set-classes! star "on" (= 5 rating))
    (.setOnAction star (w/handler (fn [e] (.consume e) (st/toggle-pick! path))))
    (.setClip tile (doto (javafx.scene.shape.Rectangle.) (-> .widthProperty (.bind (.widthProperty tile))) (-> .heightProperty (.bind (.heightProperty tile))) (.setArcWidth 8) (.setArcHeight 8)))
    (w/classes! tile "tile")
    (w/set-classes! tile "on" on?)
    (w/add! tile body num star)
    (doseq [d [:min :pref :max]] nil)
    (.setMinSize tile size size) (.setPrefSize tile size size) (.setMaxSize tile size size)
    (.setOnMouseClicked tile (w/handler (fn [e]
                                          (when (= MouseButton/PRIMARY (.getButton ^javafx.scene.input.MouseEvent e))
                                            (st/select! path)
                                            (when (= 2 (w/click-count e)) (st/go! :develop path))))))
    (thumbs/request! path (constantly true) #(.setImage iv ^Image %))
    tile))

(defn- add-tile [size on-import]
  (let [t (w/vbox 8 (w/label "+" "display") (w/tlabel "IMPORT TO THIS SHOOT" :normal "sys" "muted"))]
    (.setAlignment t Pos/CENTER)
    (.setStyle (first (.getChildren t)) "-fx-font-size: 40px; -fx-text-fill: rgba(242,233,213,0.7);")
    (let [b (doto (StackPane.) (.setAlignment Pos/CENTER))]
      (w/classes! b "tile-add")
      (w/add! b t)
      (.setMinSize b size size) (.setPrefSize b size size) (.setMaxSize b size size)
      (.setOnMouseClicked b (w/handler (fn [_] (on-import))))
      b)))

;; --------------------------------------------------------------------- view

(defn create
  "Builds the Library view. Returns {:node :refresh! :resize!}.
  `on-import` is called to open the import chooser."
  [{:keys [on-import]}]
  (let [;; sidebar
        shoots-box (w/vbox 0)
        form       (add-form)
        add-btn    (w/button (theme/tracked "NEW +" :normal) (fn [] (swap! st/state assoc :adding true)) "text-btn")
        aside      (doto (VBox.) (.setMinWidth 220) (.setPrefWidth 250))
        ;; toolbar
        title      (w/label "" "display")
        counts     (w/label "" "sys-11s" "faint")
        filters    (doto (HBox. 6.0) (.setAlignment Pos/CENTER))
        size-row   (w/slider-row {:label "SIZE" :min 120 :max 360 :step 1 :value 220 :default 220 :decimals 0
                                  :on-input (fn [v] (swap! st/state assoc :tsz (long v)))})
        dev-btn    (w/pill "DEVELOP SHOOT →" (fn [] (st/go! :develop)))
        toolbar    (doto (FlowPane. 16.0 8.0) (.setAlignment Pos/CENTER_LEFT) (.setPadding (Insets. 10 24 10 24)))
        ;; grid
        grid       (doto (FlowPane. 10.0 10.0) (.setPadding (Insets. 20)))
        empty-msg  (w/label "" "editorial")
        content    (w/add! (VBox.) grid empty-msg)
        scroll     (doto (ScrollPane. content) (.setFitToWidth true) (.setHbarPolicy ScrollPane$ScrollBarPolicy/NEVER))
        status-l   (w/tlabel "" :normal "sys-10" "faint")
        status     (w/hbox 16 status-l (w/spacer)
                           (w/tlabel "CLICK SELECT · DOUBLE-CLICK OR ↵ DEVELOP · 1–5 RATE · ☆ PICK" :normal "sys-10" "faint"))
        memo       (atom {})
        tile-size  (fn [] ; CSS grid auto-fill: as many columns as fit, then stretch to fill
                     (let [avail (max 100.0 (- (.getWidth scroll) 40.0 12.0))
                           want  (double (:tsz @st/state))
                           cols  (max 1 (long (Math/floor (/ (+ avail 10.0) (+ want 10.0)))))]
                       (/ (- avail (* 10.0 (dec cols))) cols)))
        relayout!  (fn []
                     (let [s (tile-size)]
                       (doseq [^Region t (.getChildren grid)]
                         (.setMinSize t s s) (.setPrefSize t s s) (.setMaxSize t s s))))
        rebuild-grid!
        (fn [s]
          (let [c (:catalog s) vis (st/visible-frames s) all (st/frames s)
                size (tile-size)]
            (.clear (.getChildren grid))
            (doseq [p vis]
              (w/add! grid (tile p (inc (.indexOf ^java.util.List all p)) (= p (:cur s))
                                 (cat/edited? c p) (cat/rating c p) size)))
            (w/add! grid (add-tile size on-import))
            (let [msg (cond (nil? (:shoot s)) "no shoots yet \u2014 import frames, or name a new shoot."
                            (empty? all) "this shoot is empty — import frames to begin."
                            (empty? vis) "nothing here — star a frame or change the filter.")]
              (.setText empty-msg (or msg ""))
              (.setVisible empty-msg (boolean msg)) (.setManaged empty-msg (boolean msg)))))
        main-col   (VBox.)]
    ;; static assembly
    (w/classes! aside "panel" "rule-right")
    (w/classes! toolbar "rule-bottom")
    (w/classes! status "rule-top")
    (w/classes! scroll "ground")
    (.setStyle grid "-fx-background-color: transparent;")
    (.setPadding status (Insets. 0 24 0 24))
    (.setMinHeight status 36) (.setPrefHeight status 36) (.setMaxHeight status 36)
    (w/add! aside
            (doto (w/hbox 0 (w/tlabel "SHOOTS" :wide "sys" "dim") (w/spacer) add-btn)
              (.setPadding (Insets. 8 16 0 20)))
            (:node form)
            (doto (w/vbox 0 shoots-box) (VBox/setVgrow Priority/ALWAYS))
            (doto (w/label "one folder per shoot. open it, pick, then develop." "editorial")
              (.setWrapText true) (.setPadding (Insets. 14 20 14 20)) (.setStyle "-fx-font-size: 16px; -fx-border-color: rgba(242,233,213,0.14) transparent transparent transparent; -fx-border-width: 1 0 0 0;")))
    (let [title-box (w/hbox 14 title counts)]
      (.setAlignment title-box Pos/BASELINE_LEFT)
      (w/add! toolbar title-box filters
              (w/hbox 10 (w/tlabel "SIZE" :normal "sys-10" "faint")
                      (doto (:node size-row) (.setPrefWidth 130) (.setMinWidth 130)))
              dev-btn))
    ;; the slider row shows its own label; hide the duplicate caption row
    (.setVisible (first (.getChildren ^Pane (:node size-row))) false)
    (.setManaged (first (.getChildren ^Pane (:node size-row))) false)
    (VBox/setVgrow scroll Priority/ALWAYS)
    (w/add! main-col toolbar scroll status)
    (HBox/setHgrow main-col Priority/ALWAYS)
    (.setPadding empty-msg (Insets. 0 24 12 24))
    (.setStyle empty-msg "-fx-font-size: 21px;")
    (w/add! grid)
    ;; drag & drop import onto the grid area
    (.setOnDragOver scroll (w/handler (fn [^javafx.scene.input.DragEvent e]
                                        (when (.hasFiles (.getDragboard e)) (.acceptTransferModes e (into-array TransferMode [TransferMode/COPY])))
                                        (.consume e))))
    (.setOnDragDropped scroll (w/handler (fn [^javafx.scene.input.DragEvent e]
                                           (let [files (.getFiles (.getDragboard e))
                                                 n (st/import-paths! (st/image-paths files))]
                                             (st/toast! (if (pos? (or n 0)) (str n " FRAME" (when (not= n 1) "S") " IMPORTED") "NO NEW IMAGES"))
                                             (.setDropCompleted e true) (.consume e)))))
    (.addListener (.widthProperty scroll) (w/change-listener (fn [_] (relayout!))))
    {:node (doto (w/add! (HBox.) aside main-col) (.setFillHeight true))
     :aside aside
     :empty-msg empty-msg
     :refresh!
     (fn refresh! [s]
       (let [c (:catalog s)
             shoot (cat/shoot c (:shoot s))
             fs (st/frames s) vis (st/visible-frames s)
             sig-side [(:shoots c) (:shoot s) (:adding s)]
             sig-top  [(:name shoot) (count fs) (count vis) (:lf s) (mapv #(cat/edited? c %) fs) (mapv #(cat/rating c %) fs)]
             sig-grid [vis (:lf s) (:cur s) (mapv #(cat/rating c %) vis) (mapv #(cat/edited? c %) vis) (:shoot s)]]
         (when (not= sig-side (:side @memo))
           (swap! memo assoc :side sig-side)
           (.clear (.getChildren shoots-box))
           (doseq [sh (:shoots c)]
             (w/add! shoots-box (shoot-row sh (= (:id sh) (:shoot s)))))
           (let [^Node f (:node form)]
             (.setVisible f (boolean (:adding s))) (.setManaged f (boolean (:adding s)))
             (when (:adding s) (Platform/runLater #(.requestFocus ^Node (:input form))))))
         (when (not= sig-top (:top @memo))
           (swap! memo assoc :top sig-top)
           (let [edited (count (filter #(cat/edited? c %) fs))
                 picks  (count (filter #(= 5 (cat/rating c %)) fs))]
             (.setText title (or (:name shoot) "NO SHOOT"))
             (.setText counts (theme/tracked (str (count fs) " FRAMES · " edited " EDITED · " picks " PICKS") :normal))
             (.clear (.getChildren filters))
             (doseq [[k l] [[:all (str "ALL " (count fs))] [:picks (str "PICKS " picks)] [:edited (str "EDITED " edited)]]]
               (let [b (w/pill l (fn [] (swap! st/state assoc :lf k)) "xs")]
                 (w/set-classes! b "fill-bone" (= k (:lf s)))
                 (.add (.getChildren filters) b)))
             (.setStyle title "-fx-font-size: 34px;")))
         (when (not= sig-grid (:grid @memo))
           (swap! memo assoc :grid sig-grid)
           (rebuild-grid! s))
         (.setText status-l (theme/tracked
                              (str (when (some #{(:cur s)} fs) (str (cat/frame-name (:cur s)) " · ")) (count fs) " IN SHOOT")
                              :normal))))
     :relayout! relayout!}))
