(ns darkroom.ui.view
  "JavaFX user interface. Knows nothing about how pixels are processed: it only
  hands settings to a `render-fn` and displays whatever image comes back."
  (:import (java.util.concurrent ExecutorService Executors ThreadFactory)
           (java.util.concurrent.atomic AtomicLong)
           (javafx.application Platform)
           (javafx.beans.value ChangeListener)
           (javafx.geometry Insets Pos)
           (javafx.scene Scene)
           (javafx.scene.control Label Slider)
           (javafx.scene.image ImageView WritableImage PixelFormat)
           (javafx.scene.layout BorderPane HBox Priority)
           (javafx.stage Stage)))

(defn- ->fx-image
  "Converts a pure image map into a JavaFX WritableImage."
  [{:keys [width height pixels]}]
  (let [img (WritableImage. width height)]
    (.setPixels (.getPixelWriter img) 0 0 width height
                (PixelFormat/getIntArgbInstance) ^ints pixels 0 width)
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
  [view render-fn]
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

(defn- brightness-slider []
  (doto (Slider. -100 100 0)
    (.setShowTickMarks true)
    (.setShowTickLabels true)
    (.setMajorTickUnit 50)
    (.setBlockIncrement 5)))

(defn- build-scene
  [source render-fn]
  (let [view   (doto (ImageView. (->fx-image source))
                 (.setPreserveRatio true)
                 (.setSmooth true))
        center (doto (BorderPane. view) (.setPadding (Insets. 10)))
        slider (brightness-slider)
        value  (Label. "0")
        render! (latest-wins-renderer view render-fn)
        update! (fn [v]
                  (.setText value (str (long v)))
                  (render! {:brightness (long v)}))
        bar    (doto (HBox. 10.0)
                 (.setAlignment Pos/CENTER_LEFT)
                 (.setPadding (Insets. 10))
                 (-> .getChildren (.addAll [(Label. "Brightness") slider value])))]
    (HBox/setHgrow slider Priority/ALWAYS)
    (.addListener (.valueProperty slider)
                  (reify ChangeListener
                    (changed [_ _ _ v] (update! (.doubleValue ^Number v)))))
    ;; Keep the image scaled to the window.
    (.bind (.fitWidthProperty view) (.subtract (.widthProperty center) 20))
    (.bind (.fitHeightProperty view) (.subtract (.heightProperty center) 20))
    (Scene. (doto (BorderPane.) (.setCenter center) (.setBottom bar)) 900 700)))

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
