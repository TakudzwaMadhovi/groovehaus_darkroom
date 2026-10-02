(ns darkroom.ui.fx
  "Small JavaFX helpers shared by the UI namespaces."
  (:import (java.util.concurrent ExecutorService Executors ThreadFactory)
           (javafx.scene.image PixelFormat WritableImage)))

(defn ->fx-image
  "Converts a pure image map into a JavaFX WritableImage."
  ^WritableImage [{:keys [width height pixels]}]
  (let [w (int width) h (int height)
        img (WritableImage. w h)]
    (.setPixels (.getPixelWriter img) 0 0 w h
                (PixelFormat/getIntArgbInstance) ^ints pixels 0 w)
    img))

(defn daemon-executor
  "Fixed pool of `n` daemon threads named `name`; daemon so it never blocks
  JVM exit."
  ^ExecutorService [^String name n]
  (Executors/newFixedThreadPool
    (int n)
    (reify ThreadFactory
      (newThread [_ r] (doto (Thread. ^Runnable r name) (.setDaemon true))))))
