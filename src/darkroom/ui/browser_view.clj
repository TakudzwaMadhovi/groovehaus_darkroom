(ns darkroom.ui.browser-view
  "JavaFX file browser sidebar: a folder picker and a thumbnail list. Scanning
  and thumbnailing are injected (`scan-fn`, `thumbnail-fn`) so this namespace
  only deals with presentation. Thumbnails load lazily, only for rows that
  scroll into view, on a small background pool."
  (:require [darkroom.ui.fx :as fx])
  (:import (java.io File)
           (java.util LinkedHashMap Map$Entry)
           (java.util.concurrent ExecutorService)
           (javafx.application Platform)
           (javafx.beans.value ChangeListener)
           (javafx.collections FXCollections)
           (javafx.geometry Insets Pos)
           (javafx.scene.control Button Label ListCell ListView)
           (javafx.scene.image Image ImageView)
           (javafx.scene.layout HBox Priority StackPane VBox)
           (javafx.stage DirectoryChooser Window)))

(def ^:private thumb-w 88.0)
(def ^:private thumb-h 66.0)
(def ^:private thumb-px 176) ; 2x for Retina displays

(defonce ^:private ^ExecutorService pool
  (fx/daemon-executor "darkroom-thumbs" (max 1 (min 2 (quot (.availableProcessors (Runtime/getRuntime)) 2)))))

(defonce ^:private ^ExecutorService scan-pool (fx/daemon-executor "darkroom-scan" 1))

(defn- lru-cache [limit]
  (java.util.Collections/synchronizedMap
    (proxy [LinkedHashMap] [64 0.75 true]
      (removeEldestEntry [^Map$Entry _] (> (.size ^LinkedHashMap this) limit)))))

(defn- cache-key [^File f] [(.getPath f) (.lastModified f) (.length f)])

(defn- thumbnail-loader
  "Returns (fn [file wanted? deliver!]). Delivers a cached image at once, else
  computes it on the pool, skipping work if the row was recycled meanwhile."
  [thumbnail-fn]
  (let [cache (lru-cache 400)]
    (fn [^File file wanted? deliver!]
      (if-let [img (.get ^java.util.Map cache (cache-key file))]
        (deliver! img)
        (.execute pool
                  (fn []
                    (when (wanted?)
                      (try
                        (let [img (fx/->fx-image (thumbnail-fn file))]
                          (.put ^java.util.Map cache (cache-key file) img)
                          (Platform/runLater #(deliver! img)))
                        (catch Throwable t
                          (binding [*out* *err*] (println "thumbnail failed:" (.getName file) (.getMessage t))))))))))))

(defn- make-cell [load-thumb!]
  (let [iv    (doto (ImageView.) (.setFitWidth thumb-w) (.setFitHeight thumb-h) (.setPreserveRatio true))
        frame (doto (StackPane. (into-array javafx.scene.Node [iv]))
                (.setMinSize thumb-w thumb-h) (.setPrefSize thumb-w thumb-h) (.setMaxSize thumb-w thumb-h)
                (.setStyle "-fx-background-color: #d4d4d4;"))
        label (doto (Label.) (.setWrapText true) (.setMaxWidth 110))
        box   (doto (HBox. 8.0) (.setAlignment Pos/CENTER_LEFT))
        token (atom nil)]
    (.add (.getChildren box) frame)
    (.add (.getChildren box) label)
    ;; A plain ListCell + item listener (no subclass: subclassing a JavaFX control
    ;; would initialise the toolkit when this namespace is merely loaded).
    (let [cell (ListCell.)]
      (.addListener (.itemProperty cell)
                    (reify ChangeListener
                      (changed [_ _ _ item]
                        (if (nil? item)
                          (do (reset! token nil) (.setGraphic cell nil))
                          (let [^File f item
                                mine (Object.)]
                            (reset! token mine)
                            (.setText label (.getName f))
                            (.setImage iv nil)
                            (.setGraphic cell box)
                            (load-thumb! f #(identical? mine @token)
                                         (fn [img] (when (identical? mine @token) (.setImage iv ^Image img)))))))))
      cell)))

(defn create
  "Builds the sidebar. Options:
    :scan-fn       dir -> vector of image Files
    :thumbnail-fn  (file) -> image map (longest side <= 176)
    :on-select     (fn [file]) called when the user picks an image
  Returns {:node <VBox>
           :open-dir!    (fn [dir & [file-to-highlight]]) scans in the background
           :set-status!  (fn [text])   ; FX thread}."
  [{:keys [scan-fn thumbnail-fn on-select]}]
  (let [load-thumb! (thumbnail-loader (fn [f] (thumbnail-fn f)))
        list-view   (doto (ListView.) (.setCellFactory (reify javafx.util.Callback
                                                         (call [_ _] (make-cell load-thumb!)))))
        folder      (doto (Label. "No folder") (.setMaxWidth 200) (.setStyle "-fx-font-weight: bold;"))
        count-label (Label. "")
        status      (doto (Label. "") (.setWrapText true) (.setMaxWidth 220))
        current-dir (atom nil)
        open-btn    (Button. "Open Folder…")
        node        (doto (VBox. 6.0) (.setPadding (Insets. 10)) (.setPrefWidth 250) (.setMinWidth 250))
        silent?     (atom false)
        open-dir!   (fn open-dir! [dir & [highlight]]
                      (reset! current-dir dir)
                      (.setText status "Scanning…")
                      (.execute scan-pool
                                (fn []
                                  (let [files (try (scan-fn dir) (catch Throwable _ []))]
                                    (Platform/runLater
                                      (fn []
                                        (when (= dir @current-dir)
                                          (.setText folder (.getName ^File dir))
                                          (.setText count-label (str (count files) (if (= 1 (count files)) " image" " images")))
                                          (.setText status "")
                                          (reset! silent? true)
                                          (try
                                            (.setItems list-view (FXCollections/observableArrayList ^java.util.Collection files))
                                            (when-let [i (and highlight (first (keep-indexed #(when (= (.getPath ^File %2) (.getPath ^File highlight)) %1) files)))]
                                              (.select (.getSelectionModel list-view) (int i))
                                              (.scrollTo list-view (int (max 0 (- i 2)))))
                                            (finally (reset! silent? false))))))))))]
    (VBox/setVgrow list-view Priority/ALWAYS)
    (.addListener (.selectedItemProperty (.getSelectionModel list-view))
                  (reify ChangeListener
                    (changed [_ _ _ f] (when (and f (not @silent?)) (on-select f)))))
    (.setOnAction open-btn
                  (reify javafx.event.EventHandler
                    (handle [_ _]
                      (let [chooser (doto (DirectoryChooser.) (.setTitle "Open folder of images"))
                            start   @current-dir]
                        (when (and start (.isDirectory ^File start)) (.setInitialDirectory chooser start))
                        (when-let [d (.showDialog chooser (.getWindow (.getScene open-btn)))]
                          (open-dir! d))))))
    (doseq [n [open-btn folder count-label list-view status]]
      (.add (.getChildren node) ^javafx.scene.Node n))
    {:node node
     :open-dir! open-dir!
     :set-status! #(.setText status (str %))}))
