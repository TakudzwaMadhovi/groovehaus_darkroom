(ns darkroom.main
  "Entry point: wires the image logic to the UI."
  (:require [darkroom.imaging.core :as core]
            [darkroom.imaging.pipeline :as pipeline]
            [darkroom.ui.view :as view])
  (:gen-class))

(def default-image-path "resources/sample.png")

(defn -main [& [path]]
  (let [source (core/load-image (or path default-image-path))]
    (view/show! source #(pipeline/render source (merge pipeline/default-settings %)))))
