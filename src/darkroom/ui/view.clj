(ns darkroom.ui.view
  "JavaFX user interface. Knows nothing about how pixels are processed: it only
  hands settings to the current session's `render-fn`, displays whatever image
  comes back, and hands that image to an `analyze-fn` whose result feeds the
  histogram panel. A *session* is the map describing the open image (see `show!`);
  picking a file in the browser sidebar swaps the session."
  (:require [darkroom.imaging.export :as export]
            [darkroom.ui.browser-view :as browser-view]
            [darkroom.ui.export-dialog :as export-dialog]
            [darkroom.ui.fx :as fx]
            [darkroom.ui.histogram-view :as histogram-view])
  (:import (java.util.concurrent ExecutorService)
           (java.util.concurrent.atomic AtomicLong)
           (javafx.animation PauseTransition)
           (javafx.application Platform)
           (javafx.beans.value ChangeListener)
           (javafx.geometry Insets)
           (javafx.scene Scene)
           (javafx.scene.control Alert Alert$AlertType Button ButtonType Label Slider)
           (javafx.scene.image ImageView)
           (javafx.scene.layout BorderPane ColumnConstraints GridPane Priority VBox)
           (javafx.stage Stage Window)
           (javafx.util Duration)))

;; Workers keep heavy work off the FX thread and in order.
(defonce ^:private ^ExecutorService worker (fx/daemon-executor "darkroom-render" 1))
(defonce ^:private ^ExecutorService export-worker (fx/daemon-executor "darkroom-export" 1))
(defonce ^:private ^ExecutorService open-worker (fx/daemon-executor "darkroom-open" 1))

(defn- latest-wins-renderer
  "Returns (fn [settings opts]) that renders and analyzes on the worker thread and
  shows the result on the FX thread. If newer requests arrive while one is
  queued or running, stale ones are dropped, so fast slider drags never pile
  up. Results for a session that has since been replaced are dropped too."
  [^ImageView view session analyze-fn on-analysis]
  (let [ticket (AtomicLong.)]
    (fn [settings opts]
      (let [mine     (.incrementAndGet ticket)
            sess     @session
            current? #(and (= mine (.get ticket)) (identical? sess @session))]
        (.execute worker
                  (fn []
                    (when (current?)
                      (try
                        (let [img      ((:render-fn sess) settings opts)
                              analysis (analyze-fn img)
                              fx-img   (fx/->fx-image img)]
                          (when (current?)
                            (Platform/runLater #(when (current?)
                                                  (.setImage view fx-img)
                                                  (on-analysis analysis)))))
                        (catch Throwable t (.printStackTrace t))))))))))

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
  "Export button + status line. Asks for options, then runs the current
  session's `export-fn` (settings opts -> File) on its own thread so the window
  stays responsive."
  [settings session]
  (let [button (Button. "Export…")
        status (doto (Label. "") (.setWrapText true) (.setMaxWidth 280))
        last-dir (atom nil)
        node   (doto (VBox. 6.0) (.setPadding (Insets. 10)))]
    (.setOnAction
      button
      (reify javafx.event.EventHandler
        (handle [_ _]
          (let [owner (.getWindow (.getScene button))]
            (when-let [opts (export-dialog/show owner {:dir (or @last-dir (:default-dir @session)) :name (:default-name @session)})]
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
                  (let [snapshot @settings
                        export-fn (:export-fn @session)]
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
  [{:key :denoise    :label "Denoise"    :min 0    :max 100 :value 0   :major 25 :step 5}
   {:key :brightness :label "Brightness" :min -100 :max 100 :value 0   :major 50 :step 5}
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
  "Adds a label/slider/value row to `grid`, calls (on-change key value) on
  change, and returns the Slider."
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
    (.add grid shown 2 row)
    slider))

(defn- build-scene
  "Returns {:scene Scene :swap-session! (fn [session])}. `swap-session!` must
  run on the FX thread."
  [session0 {:keys [open-fn analyze-fn scan-fn thumbnail-fn]}]
  (let [session  (atom session0)
        defaults (into {} (map (juxt :key :value)) controls)
        view     (doto (ImageView. (fx/->fx-image (:preview session0)))
                   (.setPreserveRatio true)
                   (.setSmooth true))
        ;; min/pref 0 so a large image never forces the window wider than the screen.
        center   (doto (BorderPane. view) (.setPadding (Insets. 10)) (.setMinSize 0.0 0.0) (.setPrefSize 0.0 0.0))
        settings (atom defaults)
        suppress (atom false)
        hist     (histogram-view/create)
        render!  (latest-wins-renderer view session analyze-fn (:update! hist))
        grid     (doto (GridPane.)
                   (.setHgap 10) (.setVgap 4) (.setPadding (Insets. 10)))
        ;; While a slider moves, render at :draft quality (fast); once it has been
        ;; still for 300 ms, re-render at :preview quality. Cached stages make the
        ;; second pass cheap for every adjustment except denoise itself.
        refine   (PauseTransition. (Duration/millis 300))
        _        (.setOnFinished refine
                                 (reify javafx.event.EventHandler
                                   (handle [_ _] (render! @settings {:quality :preview}))))
        sliders  (vec (for [[row spec] (map-indexed vector controls)]
                        [(slider-row grid row spec
                                     (fn [k v]
                                       (when-not @suppress
                                         (render! (swap! settings assoc k v) {:quality :draft})
                                         (.playFromStart refine))))
                         (:value spec)]))
        load-ticket (AtomicLong.)
        browser  (atom nil)
        swap-session!
        (fn [sess]
          (.stop refine)
          (reset! session sess)
          (reset! suppress true)
          (try
            (reset! settings defaults)
            (doseq [[^javafx.scene.control.Slider s v] sliders] (.setValue s (double v)))
            (finally (reset! suppress false)))
          (.setImage view (fx/->fx-image (:preview sess)))
          ((:update! hist) (analyze-fn (:preview sess)))
          (when-let [^Window w (some-> view .getScene .getWindow)]
            (.setTitle ^Stage w (str "Groovehaus Darkroom — " (:default-name sess)))))
        on-select
        (fn [^java.io.File file]
          (when-not (= (.getPath file) (some-> ^java.io.File (:file @session) .getPath))
            (let [mine     (.incrementAndGet load-ticket)
                  current? #(= mine (.get load-ticket))
                  status!  (:set-status! @browser)]
              (status! (str "Loading " (.getName file) "…"))
              (.execute open-worker
                        (fn []
                          (when (current?)
                            (try
                              (let [sess (open-fn file)]
                                (Platform/runLater #(when (current?) (swap-session! sess) (status! ""))))
                              (catch Throwable t
                                (Platform/runLater
                                  #(when (current?)
                                     (status! (str "Cannot open " (.getName file) ": " (.getMessage t))))))))))))) ]
    (reset! browser (browser-view/create {:scan-fn scan-fn :thumbnail-fn thumbnail-fn :on-select on-select}))
    (doseq [^ColumnConstraints cc [(ColumnConstraints.)
                                   (doto (ColumnConstraints.) (.setHgrow Priority/ALWAYS))
                                   (ColumnConstraints.)]]
      (.add (.getColumnConstraints grid) cc))
    ;; Keep the image scaled to the window.
    (.bind (.fitWidthProperty view) (.subtract (.widthProperty center) 20))
    (.bind (.fitHeightProperty view) (.subtract (.heightProperty center) 20))
    ((:update! hist) (analyze-fn (:preview session0)))
    (when-let [dir (:default-dir session0)]
      ((:open-dir! @browser) dir (:file session0)))
    {:swap-session! swap-session!
     :scene (Scene. (doto (BorderPane.)
                      (.setLeft (:node @browser))
                      (.setCenter center)
                      (.setRight (row-of (VBox.) (:node hist) (export-panel settings session)))
                      (.setBottom grid))
                    1400 850)}))

(defn show!
  "Starts the JavaFX runtime (if needed) and opens the main window.
  A session map describes the open image:
    :file         java.io.File of the image
    :preview      pure image map shown on screen
    :render-fn    settings map -> image map
    :export-fn    (settings, {:dir :name :format :quality}) -> File written
    :default-name suggested export file name (no extension)
    :default-dir  folder shown in the browser sidebar / suggested for export
  Options:
    :session       the initial session
    :open-fn       File -> session; called on a background thread
    :analyze-fn    rendered image map -> histogram data
    :scan-fn       dir -> vector of image Files for the sidebar
    :thumbnail-fn  File -> thumbnail image map"
  [{:keys [session] :as opts}]
  (let [open! (fn []
                (let [{:keys [scene]} (build-scene session opts)]
                  (doto (Stage.)
                    (.setTitle (str "Groovehaus Darkroom — " (:default-name session)))
                    (.setScene scene)
                    (.show))))]
    (try
      (Platform/startup ^Runnable open!)
      (catch IllegalStateException _ ; toolkit already running (e.g. REPL)
        (Platform/runLater open!)))))
