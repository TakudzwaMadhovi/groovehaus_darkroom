(ns darkroom.ui.library
  "Library view: shoots sidebar, toolbar (filters, size), thumbnail grid and
  status bar. Reads state, calls actions; owns no data."
  (:require [clojure.string :as str]
            [darkroom.catalog :as cat]
            [darkroom.remote :as remote]
            [darkroom.ui.info :as info]
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

(defonce grid-columns (atom 1))

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
        n-lbl (w/tlabel (str (count paths) (if (= 1 (count paths)) " FRAME" " FRAMES")) :normal "sys-10" "faint")
        text  (w/vbox 2 label n-lbl)
        row   (doto (Button.) (.setMaxWidth Double/MAX_VALUE))]
    (.setClip cover (doto (javafx.scene.shape.Rectangle. 44 44) (.setArcWidth 8) (.setArcHeight 8)))
    (w/add! cover iv)
    (.setMaxWidth label 150.0)
    (.setTextOverrun label javafx.scene.control.OverrunStyle/ELLIPSIS)
    (when-let [p (first paths)]
      (thumbs/request! p (constantly true) #(crop-square! iv %)))
    (w/classes! row "shoot-row")
    (w/set-on! row on?)
    (w/a11y! row (str name ", " (count paths) (if (= 1 (count paths)) " frame" " frames") (when on? ", current shoot")))
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

(defn- tile [path idx on? edited? rating size colour rejected?]
  (let [area  (doto (StackPane.) (.setPadding (Insets. 22 10 4 10)) (.setMinSize 0 0) (.setPrefSize 0 0))
        iv    (doto (ImageView.) (.setPreserveRatio true) (.setSmooth true))
        name  (w/tlabel (cat/frame-name path) :tight "sys" "sys-10")
        meta  (w/tlabel (str (when rejected? "REJECTED · ") (when edited? "EDITED · ") (dots rating)) :tight "sys-10" "sys" "dim")
        dot   (when colour
                (doto (javafx.scene.shape.Circle. 5.0) (.setFill (javafx.scene.paint.Color/web (theme/label-colours colour)))))
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
    (w/add! bar dot name (w/spacer) meta)
    (w/add! body area bar)
    (StackPane/setAlignment num Pos/TOP_LEFT)
    (StackPane/setMargin num (Insets. 6 0 0 8))
    (.setMouseTransparent num true)
    (StackPane/setAlignment star Pos/TOP_RIGHT)
    (w/set-classes! star "on" (= 5 rating))
    (.setOnAction star (w/handler (fn [e] (.consume e) (st/toggle-pick! path))))
    (w/a11y! star (str (if (= 5 rating) "Remove pick from " "Pick ") (cat/frame-name path)))
    (.setClip tile (doto (javafx.scene.shape.Rectangle.) (-> .widthProperty (.bind (.widthProperty tile))) (-> .heightProperty (.bind (.heightProperty tile))) (.setArcWidth 8) (.setArcHeight 8)))
    (w/classes! tile "tile")
    (w/set-classes! tile "on" on?)
    (.setFocusTraversable tile true)
    (.setAccessibleRole tile javafx.scene.AccessibleRole/BUTTON)
    (w/a11y! tile (str (cat/frame-name path) ", frame " idx ", rating " rating " of 5"
                       (when colour (str ", " (clojure.core/name colour) " label")) (when rejected? ", rejected")
                       (when edited? ", edited") (when on? ", selected")))
    (when rejected? (.setOpacity tile 0.4))
    (.setAccessibleHelp tile "Space selects. Control or shift click selects several. Enter opens in Develop. Arrow keys move between frames.")
    (.setOnKeyPressed tile (w/handler (fn [^KeyEvent e]
                                        (when (= (.getCode e) KeyCode/SPACE) (st/select! path) (.consume e)))))
    (w/add! tile body num star)
    (.setMinSize tile size size) (.setPrefSize tile size size) (.setMaxSize tile size size)
    (.setOnMouseClicked tile (w/handler (fn [e]
                                          (let [^javafx.scene.input.MouseEvent e e]
                                            (when (= MouseButton/PRIMARY (.getButton e))
                                              (cond (.isShortcutDown e) (st/toggle-select! path)
                                                    (.isShiftDown e) (st/range-select! path)
                                                    :else (do (st/select! path)
                                                              (when (= 2 (w/click-count e)) (st/go! :develop path)))))))))
    (thumbs/request! path (constantly true) #(.setImage iv ^Image %))
    tile))

(defn- add-tile [size on-import]
  (let [t (w/vbox 8 (w/label "+" "display") (w/tlabel "IMPORT TO THIS SHOOT" :normal "sys" "muted"))
        b (doto (Button.) (.setAlignment Pos/CENTER))]
    (.setAlignment t Pos/CENTER)
    (.setStyle (first (.getChildren t)) "-fx-font-size: 40px; -fx-text-fill: rgba(242,233,213,0.7);")
    (w/classes! b "gh-btn" "tile-add")
    (.setGraphic b t)
    (.setContentDisplay b javafx.scene.control.ContentDisplay/GRAPHIC_ONLY)
    (w/a11y! b "Import images to this shoot")
    (.setMinSize b size size) (.setPrefSize b size size) (.setMaxSize b size size)
    (.setOnAction b (w/handler (fn [_] (on-import))))
    b))

;; --------------------------------------------------------------------- view

(def ^:private sorts [:import :name :rating :capture :edited])

(defn- next-sort [cur] (let [i (.indexOf ^java.util.List sorts (or cur :import))] (nth sorts (mod (inc i) (count sorts)))))

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
        filter-btns (into {} (for [k [:all :picks :edited :rejected]]
                               [k (w/pill (name k) (fn [] (swap! st/state assoc :lf k)) "xs")]))
        size-row   (w/slider-row {:label "SIZE" :min 120 :max 360 :step 1 :value 220 :default 220 :decimals 0
                                  :on-input (fn [v] (swap! st/state assoc :tsz (long v)))})
        dev-btn    (w/pill "DEVELOP SHOOT →" (fn [] (st/go! :develop)))
        toolbar    (doto (FlowPane. 16.0 8.0) (.setAlignment Pos/CENTER_LEFT) (.setPadding (Insets. 10 24 10 24)))
        ;; search / sort / colour filter row
        search     (doto (TextField.) (.setPromptText "search name, keyword, camera…") (.setPrefWidth 260))
        sort-btn   (doto (w/pill "SORT" (fn [] (st/set-query! :sort (next-sort (:sort (:query @st/state))))) "xs")
                     (w/set-base-a11y! "Change sort order"))
        dir-btn    (doto (w/pill "↑" (fn [] (st/set-query! :dir (if (= :desc (:dir (:query @st/state))) :asc :desc))) "xs")
                     (w/set-base-a11y! "Reverse sort direction"))
        colour-btns (into {} (for [c cat/colour-labels]
                               [c (doto (Button.)
                                    (w/classes! "gh-btn" "swatch")
                                    (.setGraphic (doto (javafx.scene.shape.Circle. 7.0)
                                                   (.setFill (javafx.scene.paint.Color/web (theme/label-colours c)))))
                                    (w/set-base-a11y! (str "Show only " (name c) " label"))
                                    (.setOnAction (w/handler (fn [_] (st/set-query! :colour (when (not= c (:colour (:query @st/state))) c))))))]))
        survey-btn (doto (w/pill "SURVEY" (fn [] (swap! st/state update :survey not)) "xs")
                     (w/set-base-a11y! "Survey: show the selected frames large (N)"))
        watch-btn  (doto (w/pill "WATCH FOLDER" (fn []
                                                  (if (:watch @st/state)
                                                    (do (st/stop-watching!) (st/toast! "STOPPED WATCHING"))
                                                    (let [ch (doto (javafx.stage.DirectoryChooser.) (.setTitle "Watch a folder for new frames"))]
                                                      (when-let [d (.showDialog ch (first (javafx.stage.Window/getWindows)))]
                                                        (st/watch-folder! d)
                                                        (st/toast! (str "WATCHING " (.toUpperCase (.getName d))))))))
                                         "xs")
                     (w/set-base-a11y! "Watch a folder: new frames are imported as they are saved"))
        toolbar2   (doto (FlowPane. 14.0 6.0) (.setAlignment Pos/CENTER_LEFT) (.setPadding (Insets. 6 24 6 24)))
        ;; metadata / actions panel
        info-panel (info/create)
        phone-qr   (doto (ImageView.) (.setFitWidth 200.0) (.setFitHeight 200.0) (.setSmooth false))
        phone-url  (doto (w/classes! (TextField.) "gh-input") (.setEditable false) (w/a11y! "Phone companion address"))
        phone-box  (doto (w/vbox 8
                                 phone-qr phone-url
                                 (doto (w/label "scan with your phone's camera (same wi-fi). anyone on this network who has the address can rate and flag your photos: switch it off when done." "editorial")
                                   (.setWrapText true) (.setStyle "-fx-font-size: 15px;")))
                     (.setVisible false) (.setManaged false))
        act-pills  (let [fp (FlowPane. 8.0 8.0)
                         p  (fn [text f a11y] (doto (w/pill text f "xs") (w/set-base-a11y! a11y)))
                         n! (fn [n word] (st/toast! (str n " " word)))]
                     (w/add! fp
                             (p "COPY" (fn [] (when (st/copy-settings!) (st/toast! "SETTINGS COPIED"))) "Copy settings of the current frame")
                             (p "PASTE" (fn [] (if-let [n (st/paste-settings!)] (n! n "PASTED") (st/toast! "COPY SETTINGS FIRST"))) "Paste settings onto the selection")
                             (p "VIRTUAL COPY" (fn [] (when-let [n (st/virtual-copy!)] (n! n "COPIES MADE"))) "Make a virtual copy of the selection")
                             (p "REMOVE" (fn [] (when-let [n (st/remove-frames!)] (n! n "REMOVED FROM SHOOT"))) "Remove the selection from the shoot (files are kept)")
                             (p "WRITE XMP" (fn [] (n! (st/write-xmp!) "SIDECARS WRITTEN")) "Write XMP sidecars for the shoot")
                             (p "HDR MERGE" (fn [] (st/merge-hdr!)) "Merge the selected frames (an exposure bracket) into one HDR frame")
                             (p "PANORAMA" (fn [] (st/stitch-panorama!)) "Stitch the selected frames into a panorama")
                             (doto (w/pill "PHONE" (fn [] (if (st/remote-running?) (st/stop-remote!) (st/start-remote!))) "xs")
                               (w/set-base-a11y! "Switch the phone companion on or off: browse and rate from a phone on the same network"))))
        ;; WebDAV catalog sync
        mk-field   (fn [prompt a11y]
                     (doto (w/classes! (TextField.) "gh-input") (.setPromptText prompt) (w/a11y! a11y)))
        sync-url   (mk-field "https://server/remote.php/dav/files/me/groovehaus" "Sync folder address")
        sync-user  (mk-field "user name" "Sync user name")
        sync-pass  (doto (w/classes! (javafx.scene.control.PasswordField.) "gh-input") (.setPromptText "password (not saved)") (w/a11y! "Sync password"))
        sync-map   (mk-field "this=shared; /Users/me/Photos=D:/Photos" "Photo path map (optional)")
        sync-save! (fn [] (st/set-sync! (.getText sync-url) (.getText sync-user) (.getText sync-map)))
        sync-box   (let [commit (fn [] (sync-save!) (reset! st/sync-password (.getText sync-pass)))]
                     (doseq [^TextField f [sync-url sync-user sync-pass sync-map]]
                       (.addListener (.focusedProperty f) (w/change-listener (fn [focused] (when-not focused (commit)))))
                       (.setOnKeyPressed f (w/handler (fn [^KeyEvent e] (when (= (.getCode e) KeyCode/ENTER) (commit))))))
                     (w/vbox 8 sync-url sync-user sync-pass sync-map
                             (doto (FlowPane. 8.0 8.0)
                               (w/add! (doto (w/pill "UPLOAD" (fn [] (commit) (st/sync-push!)) "xs")
                                         (w/set-base-a11y! "Upload the catalog to the sync folder"))
                                       (doto (w/pill "DOWNLOAD" (fn [] (commit) (st/sync-pull!)) "xs")
                                         (w/set-base-a11y! "Download the catalog from the sync folder, replacing this one (the old one is backed up)"))))))
        side       (doto (VBox. 18.0) (.setPadding (Insets. 18 20 18 20)) (.setMinWidth 260) (.setPrefWidth 280))
        side-scroll (doto (ScrollPane. side) (.setFitToWidth true) (.setHbarPolicy ScrollPane$ScrollBarPolicy/NEVER)
                      (.setMinWidth 260) (.setPrefWidth 280) (.setMaxWidth 320))
        ;; grid
        grid       (doto (FlowPane. 10.0 10.0) (.setPadding (Insets. 20)))
        empty-msg  (w/label "" "editorial")
        content    (w/add! (VBox.) grid empty-msg)
        scroll     (doto (ScrollPane. content) (.setFitToWidth true) (.setHbarPolicy ScrollPane$ScrollBarPolicy/NEVER))
        status-l   (w/tlabel "" :normal "sys-10" "faint")
        status     (w/hbox 16 status-l (w/spacer)
                           (w/tlabel "CLICK SELECT · ⌘/SHIFT-CLICK MULTI · ↵ DEVELOP · 1–5 RATE · X REJECT · 6–9 LABEL" :normal "sys-10" "faint"))
        memo       (atom {})
        tile-size  (fn [] ; CSS grid auto-fill: as many columns as fit, then stretch to fill
                     (let [s     @st/state
                           avail (max 100.0 (- (.getWidth scroll) 40.0 12.0))
                           want  (double (:tsz s))
                           n     (count (st/grid-frames s))]
                       (if (st/survey? s)
                         ;; survey: the selected frames as large as they fit without scrolling
                         (let [cols (long (Math/ceil (Math/sqrt n)))
                               rows (long (Math/ceil (/ n (double cols))))
                               by-w (/ (- avail (* 10.0 (dec cols))) cols)
                               by-h (/ (- (max 100.0 (- (.getHeight scroll) 40.0)) (* 10.0 (dec rows))) rows)]
                           (reset! grid-columns cols)
                           (max 100.0 (min by-w by-h)))
                         (let [cols (max 1 (long (Math/floor (/ (+ avail 10.0) (+ want 10.0)))))]
                           (reset! grid-columns cols)
                           (/ (- avail (* 10.0 (dec cols))) cols)))))
        relayout!  (fn []
                     (let [s (tile-size)]
                       (doseq [^Region t (.getChildren grid)]
                         (.setMinSize t s s) (.setPrefSize t s s) (.setMaxSize t s s))))
        rebuild-grid!
        (fn [s]
          (let [c (:catalog s) vis (st/grid-frames s) all (st/frames s)
                size (tile-size)]
            ;; keyboard focus follows the selected frame across rebuilds
            (w/keep-focus!
              grid
              (fn []
                (.clear (.getChildren grid))
                (doseq [p vis]
                  (w/add! grid (tile p (inc (.indexOf ^java.util.List all p)) (or (= p (:cur s)) (contains? (:sel s) p))
                                     (cat/edited? c p) (cat/rating c p) size (cat/colour c p) (cat/rejected? c p))))
                (when-not (st/survey? s) (w/add! grid (add-tile size on-import))))
              (fn [g] (let [i (.indexOf ^java.util.List vis (:cur @st/state))]
                        (when (>= i 0) (.get (.getChildren g) i)))))
            (let [msg (cond (nil? (:shoot s)) "no shoots yet \u2014 import frames, or name a new shoot."
                            (empty? all) "this shoot is empty — import frames to begin."
                            (empty? vis) "nothing here — change the search or filter.")]
              (.setText empty-msg (or msg ""))
              (.setVisible empty-msg (boolean msg)) (.setManaged empty-msg (boolean msg)))))
        main-col   (VBox.)]
    ;; static assembly
    (apply w/add! filters (map filter-btns [:all :picks :edited :rejected]))
    (w/a11y! (:slider size-row) "Thumbnail size")
    (w/classes! aside "panel" "rule-right")
    (w/classes! toolbar "rule-bottom")
    (w/classes! toolbar2 "rule-bottom")
    (w/classes! search "gh-input")
    (w/a11y! search "Search the shoot")
    (.addListener (.textProperty search) (w/change-listener (fn [t] (st/set-query! :text (when-not (clojure.string/blank? t) t)))))
    (apply w/add! toolbar2 search sort-btn dir-btn (concat (map colour-btns cat/colour-labels) [survey-btn watch-btn]))
    (w/classes! side-scroll "panel" "rule-left")
    (.setStyle side-scroll "-fx-background-color: transparent;")
    (w/add! side (w/tlabel "INFO" :wide "sys" "dim") (:node info-panel) (w/tlabel "ACTIONS" :wide "sys" "dim") act-pills phone-box
            (w/tlabel "SYNC CATALOG (WEBDAV)" :wide "sys" "dim") sync-box)
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
    (w/add! main-col toolbar toolbar2 scroll status)
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
    {:node (doto (w/add! (HBox.) aside main-col side-scroll) (.setFillHeight true))
     :aside aside
     :empty-msg empty-msg
     :refresh!
     (fn refresh! [s]
       (let [c (:catalog s)
             shoot (cat/shoot c (:shoot s))
             fs (st/frames s) vis (st/grid-frames s)
             sig-side [(:shoots c) (:shoot s) (:adding s)]
             sig-top  [(:name shoot) (count fs) (count vis) (:lf s) (:query s) (mapv #(cat/edited? c %) fs) (mapv #(cat/rating c %) fs)
                       (mapv #(cat/rejected? c %) fs)]
             sig-grid [vis (st/survey? s) (:cur s) (:sel s) (mapv #(cat/rating c %) vis) (mapv #(cat/edited? c %) vis)
                       (mapv #(cat/colour c %) vis) (mapv #(cat/rejected? c %) vis) (:shoot s)]]
         (when (not= sig-side (:side @memo))
           (swap! memo assoc :side sig-side)
           (w/keep-focus! shoots-box
                          (fn []
                            (.clear (.getChildren shoots-box))
                            (doseq [sh (:shoots c)]
                              (w/add! shoots-box (shoot-row sh (= (:id sh) (:shoot s)))))))
           (let [^Node f (:node form)]
             (.setVisible f (boolean (:adding s))) (.setManaged f (boolean (:adding s)))
             (when (:adding s) (Platform/runLater #(.requestFocus ^Node (:input form))))))
         (when (not= sig-top (:top @memo))
           (swap! memo assoc :top sig-top)
           (st/load-meta!)
           (let [edited (count (filter #(cat/edited? c %) fs))
                 picks  (count (filter #(= 5 (cat/rating c %)) fs))
                 rejected (count (filter #(cat/rejected? c %) fs))
                 q      (:query s)]
             (.setText title (or (:name shoot) "NO SHOOT"))
             (.setText counts (theme/tracked (str (count fs) " FRAMES · " edited " EDITED · " picks " PICKS") :normal))
             (doseq [[k l] [[:all (str "ALL " (- (count fs) rejected))] [:picks (str "PICKS " picks)] [:edited (str "EDITED " edited)]
                          [:rejected (str "REJECTED " rejected)]]]
               (let [^Button b (get filter-btns k) on? (= k (:lf s))]
                 (.setText b (theme/tracked l :normal))
                 (w/set-classes! b "fill-bone" on?)
                 (w/a11y! b (str "Filter: " (.toLowerCase l) (when on? ", selected")))))
             (.setText sort-btn (theme/tracked (str "SORT: " (name (or (:sort q) :import))) :normal))
             (.setText dir-btn (if (= :desc (:dir q)) "↓" "↑"))
             (doseq [[col ^Button b] colour-btns] (w/set-on! b (= col (:colour q))))
             (.setStyle title "-fx-font-size: 34px;")))
         (let [t (str (:text (:query s)))]
           (when (and (not (.isFocused search)) (not= t (.getText search))) (.setText search t)))
         (let [{:keys [url user path-map]} (:sync c)
               put-text! (fn [^TextField f t] (when (and (not (.isFocused f)) (not= t (.getText f))) (.setText f t)))]
           (put-text! sync-url (str url)) (put-text! sync-user (str user))
           (put-text! sync-map (clojure.string/join "; " (map (fn [[a b]] (str a "=" b)) path-map))))
         (let [url (:remote-url s) show? (boolean url)]
           (.setVisible phone-box show?) (.setManaged phone-box show?)
           (when (and url (not= url (.getText phone-url)))
             (.setText phone-url url)
             (let [{:keys [size on?]} (remote/qr url) k 6 n (* size k)
                   img (javafx.scene.image.WritableImage. (int n) (int n))
                   pw (.getPixelWriter img)]
               (dotimes [y n] (dotimes [x n] (.setArgb pw x y (if (on? (quot x k) (quot y k)) (unchecked-int 0xFF000000) (unchecked-int 0xFFFFFFFF)))))
               (.setImage phone-qr img))))
         (w/set-on! survey-btn (st/survey? s))
         (.setText watch-btn (theme/tracked (if (:watch s) "WATCHING ■" "WATCH FOLDER") :normal))
         (w/set-on! watch-btn (boolean (:watch s)))
         (.setDisable survey-btn (and (not (:survey s)) (< (count (:sel s)) 2)))
         ((:sync! info-panel) s)
         (when (not= sig-grid (:grid @memo))
           (swap! memo assoc :grid sig-grid)
           (rebuild-grid! s))
         (.setText status-l (theme/tracked
                              (str (when (some #{(:cur s)} fs) (str (cat/frame-name (:cur s)) " · ")) (count fs) " IN SHOOT")
                              :normal))))
     :relayout! relayout!}))
