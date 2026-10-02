(ns groovehaus.looks.fx
  "JavaFX bridge. Resolved reflectively so the looks engine has no hard JavaFX dependency
  (and stays unit-testable headless)."
  (:require [groovehaus.looks.core :as looks]))

(defn buffered->fx
  "BufferedImage -> javafx.scene.image.WritableImage (needs the javafx.swing module)."
  [bi]
  (clojure.lang.Reflector/invokeStaticMethod
   "javafx.embed.swing.SwingFXUtils" "toFXImage" (object-array [bi nil])))

(defn fx->buffered [fx-image]
  (clojure.lang.Reflector/invokeStaticMethod
   "javafx.embed.swing.SwingFXUtils" "fromFXImage" (object-array [fx-image nil])))

(defn fx-renderer
  "Like looks/renderer but takes and returns JavaFX Images."
  [fx-image]
  (let [render (looks/renderer (fx->buffered fx-image))]
    (fn [stack] (buffered->fx (render stack)))))
