(ns darkroom.ui.view
  "JavaFX user interface. Knows nothing about how pixels are processed: it only
  hands settings to a `render-fn`, displays whatever image comes back, and
  hands that image to an `analyze-fn` whose result feeds the histogram panel."
  (:require [darkroom.imaging.export :as export]
            [darkroom.ui.export-dialog :as export-dialog]
            [darkroom.ui.histogram-view :as histogram-view])
  (:import (java.util.concurrent ExecutorService Executors ThreadFactory)
           (java.util.concurrent.atomic AtomicLong)
           (javafx.application Platform)
           (javafx.beans.value ChangeListener)
           (javafx.geometry Insets)
           (javafx.scene Scene)
           (javafx.scene.control Alert Alert$AlertType Button ButtonType Label Slider)
           (javafx.scene.image ImageView WritableImage PixelFormat)
           (javafx.scene.layout BorderPane ColumnConstraints GridPane Priority VBox)
           (javafx.stage Stage Window)))

(defn- ->fx-image
  "Converts a pure image map into a JavaFX WritableImage."
  ^WritableImage [{:keys [width height pixels]}]
  (let [w (int width) h (int height)
        img (WritableImage. w h)]
    (.setPixels (.getPixelWriter img) 0 0 w h
                (PixelFormat/getIntArgbInstance) ^ints pixels 0 w)
    img))

;; One daemon worker keeps renders off the FX thread and in order; daemon so it
;; never blocks JVM exit.
(defonce ^:private ^ExecutorService worker
  (Executors/newSingleThreadExecutor
    (reify ThreadFactory
      (newThread [_ r] (doto (Thread. ^Runnable r "darkroom-render") (.setDaemon true))))))

(defn- latest-wins-renderer
  "Returns (fn [settings]) that renders and analyzes on the worker thread and
  shows the result on the FX thread. If newer requests arrive while one is
  queued or running, stale ones are dropped, so fast slider drags never pile up."
  [^ImageView view render-fn analyze-fn on-analysis]
  (let [ticket (AtomicLong.)]
    (fn [settings]
      (let [mine (.incrementAndGet ticket)
            current? #(= mine (.get ticket))]
        (.execute worker
                  (fn []
                    (when (current?)
                      (try
                        (let [img      (render-fn settings)
                              analysis (analyze-fn img)
                              fx-img   (->fx-image img)]
                          (when (current?)
                            (Platform/runLater #(when (current?)
                                                  (.setImage view fx-img)
                                                  (on-analysis analysis)))))
                        (catch Throwable t (.printStackTrace t))))))))))

(defonce ^:private ^ExecutorService export-worker
  (Executors/newSingleThreadExecutor
    (reify ThreadFactory
      (newThread [_ r] (doto (Thread. ^Runnable r "darkroom-export") (.setDaemon true))))))

(defn- row-of
  "Adds `nodes` to the children of a VBox/HBox and returns it."
  [^javafx.scene.layout.Pane pane & nodes]
  (doseq [n nodes] (.add (.getChildren pane) ^javafx.scene.Node n))
  pane)

(defn- alert [type ^String msg ^Window owner]
  (doto (Alert. type msg (into-array ButtonType [ButtonType/OK]))
    (.initOwner owner)
    (.showAndWait)))

(defn- export-panel
  "Export button + status line. Asks for options, then runs `export-fn`
  (settings opts -> File) on its own thread so the window stays responsive."
  [settings export-fn {:keys [default-name default-dir]}]
  (let [button (Button. "Export…")
        status (doto (Label. "") (.setWrapText true) (.setMaxWidth 280))
        last-dir (atom default-dir)
        node   (doto (VBox. 6.0) (.setPadding (Insets. 10)))]
    (.setOnAction
      button
      (reify javafx.event.EventHandler
        (handle [_ _]
          (let [owner (.getWindow (.getScene button))]
            (when-let [opts (export-dialog/show owner {:dir @last-dir :name default-name})]
              (let [target (try (export/target-file (:dir opts) (:name opts) (:format opts))
                                (catch clojure.lang.ExceptionInfo e
                                  (alert Alert$AlertType/ERROR (.getMessage e) owner) nil))
                    ok?    (and target
                                (or (not (.exists ^java.io.File target))
                                    (= ButtonType/OK
                                       (.orElse (.showAndWait
                                                  (doto (Alert. Alert$AlertType/CONFIRMATION
                                                                (str target " already exists. Replace it?")
                                                                (into-array ButtonType [ButtonType/OK ButtonType/CANCEL]))
                                                    (.initOwner owner)))
                                                ButtonType/CANCEL))))]
                (when ok?
                  (reset! last-dir (:dir opts))
                  (.setDisable button true)
                  (.setText status "Exporting…")
                  (let [snapshot @settings]
                    (.execute export-worker
                              (fn []
                                (try
                                  (let [f (export-fn snapshot opts)]
                                    (Platform/runLater
                                      #(do (.setText status (str "Saved " f)) (.setDisable button false))))
                                  (catch Throwable t
                                    (.printStackTrace t)
                                    (Platform/runLater
                                      #(do (.setText status "Export failed")
                                           (.setDisable button false)
                                           (alert Alert$AlertType/ERROR
                                                  (str "Export failed: " (.getMessage t)) owner)))))))))))))))
    (.addAll (.getChildren node) ^"[Ljavafx.scene.Node;" (into-array javafx.scene.Node [button status]))
    node))

(def controls
  "One slider per adjustment. :key is the setting name handed to render-fn."
  [{:key :brightness :label "Brightness" :min -100 :max 100 :value 0   :major 50 :step 5}
   {:key :contrast   :label "Contrast"   :min -100 :max 100 :value 0   :major 50 :step 5}
   {:key :saturation :label "Saturation" :min -100 :max 100 :value 0   :major 50 :step 5}
   {:key :gamma      :label "Gamma"      :min 0.2  :max 3.0 :value 1.0 :major 0.8 :step 0.05 :decimals 2}])

(defn- fmt [{:keys [decimals]} v]
  (if decimals (format (str "%." decimals "f") (double v)) (str (Math/round (double v)))))

(defn- round-to [{:keys [decimals]} v]
  (if decimals
    (let [m (Math/pow 10 decimals)] (/ (Math/round (* (double v) m)) m))
    (Math/round (double v))))

(defn- slider-row
  "Adds a label/slider/value row to `grid`. Calls (on-change key value)."
  [^GridPane grid row {:keys [key label min max value major step] :as spec} on-change]
  (let [slider (doto (Slider. min max value)
                 (.setShowTickMarks true)
                 (.setShowTickLabels true)
                 (.setMajorTickUnit major)
                 (.setBlockIncrement step))
        shown  (doto (Label. (fmt spec value)) (.setMinWidth 40))]
    (.addListener (.valueProperty slider)
                  (reify ChangeListener
                    (changed [_ _ _ v]
                      (let [v (round-to spec (.doubleValue ^Number v))]
                        (.setText shown (fmt spec v))
                        (on-change key v)))))
    ;; double-click a slider to reset it
    (.setOnMouseClicked slider
                        (reify javafx.event.EventHandler
                          (handle [_ e]
                            (when (= 2 (.getClickCount ^javafx.scene.input.MouseEvent e))
                              (.setValue slider value)))))
    (.add grid (Label. label) 0 row)
    (.add grid slider 1 row)
    (.add grid shown 2 row)))

(defn- build-scene
  [source render-fn analyze-fn export-fn export-defaults]
  (let [view     (doto (ImageView. (->fx-image source))
                   (.setPreserveRatio true)
                   (.setSmooth true))
        ;; min/pref 0 so a large image never forces the window wider than the screen.
        center   (doto (BorderPane. view) (.setPadding (Insets. 10)) (.setMinSize 0.0 0.0) (.setPrefSize 0.0 0.0))
        settings (atom (into {} (map (juxt :key :value)) controls))
        hist     (histogram-view/create)
        render!  (latest-wins-renderer view render-fn analyze-fn (:update! hist))
        grid     (doto (GridPane.)
                   (.setHgap 10) (.setVgap 4) (.setPadding (Insets. 10)))]
    (doseq [^ColumnConstraints cc [(ColumnConstraints.)
                                   (doto (ColumnConstraints.) (.setHgrow Priority/ALWAYS))
                                   (ColumnConstraints.)]]
      (.add (.getColumnConstraints grid) cc))
    (doseq [[row spec] (map-indexed vector controls)]
      (slider-row grid row spec
                  (fn [k v] (render! (swap! settings assoc k v)))))
    ;; Keep the image scaled to the window.
    (.bind (.fitWidthProperty view) (.subtract (.widthProperty center) 20))
    (.bind (.fitHeightProperty view) (.subtract (.heightProperty center) 20))
    ((:update! hist) (analyze-fn source))
    (Scene. (doto (BorderPane.)
              (.setCenter center)
              (.setRight (row-of (VBox.) (:node hist) (export-panel settings export-fn export-defaults)))
              (.setBottom grid))
            1200 800)))

(defn show!
  "Starts the JavaFX runtime (if needed) and opens the main window.
  Options:
    :preview      pure image map shown on screen
    :render-fn    settings map -> image map
    :analyze-fn   rendered image map -> histogram data
    :export-fn    (settings, {:dir :name :format :quality}) -> File written
    :default-name suggested export file name (no extension)
    :default-dir  suggested export folder (File)"
  [{:keys [preview render-fn analyze-fn export-fn default-name default-dir]}]
  (let [open! (fn []
                (doto (Stage.)
                  (.setTitle "Groovehaus Darkroom")
                  (.setScene (build-scene preview render-fn analyze-fn export-fn
                                          {:default-name default-name :default-dir default-dir}))
                  (.show)))]
    (try
      (Platform/startup ^Runnable open!)
      (catch IllegalStateException _ ; toolkit already running (e.g. REPL)
        (Platform/runLater open!)))))
