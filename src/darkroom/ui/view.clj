(ns darkroom.ui.view
  "JavaFX user interface. Knows nothing about how pixels are processed: it only
  hands settings to a `render-fn` and displays whatever image comes back."
  (:import (java.util.concurrent ExecutorService Executors ThreadFactory)
           (java.util.concurrent.atomic AtomicLong)
           (javafx.application Platform)
           (javafx.beans.value ChangeListener)
           (javafx.geometry Insets)
           (javafx.scene Scene)
           (javafx.scene.control Label Slider)
           (javafx.scene.image ImageView WritableImage PixelFormat)
           (javafx.scene.layout BorderPane ColumnConstraints GridPane Priority)
           (javafx.stage Stage)))

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
  "Returns (fn [settings]) that renders on the worker thread and shows the
  result on the FX thread. If newer requests arrive while one is queued or
  running, stale ones are dropped, so fast slider drags never pile up."
  [^ImageView view render-fn]
  (let [ticket (AtomicLong.)]
    (fn [settings]
      (let [mine (.incrementAndGet ticket)
            current? #(= mine (.get ticket))]
        (.execute worker
                  (fn []
                    (when (current?)
                      (try
                        (let [fx-img (->fx-image (render-fn settings))]
                          (when (current?)
                            (Platform/runLater #(when (current?) (.setImage view fx-img)))))
                        (catch Throwable t (.printStackTrace t))))))))))

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
  [source render-fn]
  (let [view     (doto (ImageView. (->fx-image source))
                   (.setPreserveRatio true)
                   (.setSmooth true))
        center   (doto (BorderPane. view) (.setPadding (Insets. 10)))
        settings (atom (into {} (map (juxt :key :value)) controls))
        render!  (latest-wins-renderer view render-fn)
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
    (Scene. (doto (BorderPane.) (.setCenter center) (.setBottom grid)) 900 800)))

(defn show!
  "Starts the JavaFX runtime (if needed) and opens the main window.
  `source` is a pure image map; `render-fn` takes a settings map and returns
  an image map."
  [source render-fn]
  (let [open! (fn []
                (doto (Stage.)
                  (.setTitle "Groovehaus Darkroom")
                  (.setScene (build-scene source render-fn))
                  (.show)))]
    (try
      (Platform/startup ^Runnable open!)
      (catch IllegalStateException _ ; toolkit already running (e.g. REPL)
        (Platform/runLater open!)))))
