(ns darkroom.main
  "Entry point: wires the image logic to the UI."
  (:require [darkroom.imaging.core :as core]
            [darkroom.imaging.pipeline :as pipeline]
            [darkroom.ui.view :as view])
  (:gen-class))

(def default-image-path "resources/sample.png")

(def preview-max-side
  "Longest side of the on-screen proxy. Sliders re-render this, not the
  full-resolution original (kept in `source` for future export)."
  1600)

(defn -main [& [path]]
  (let [source  (core/load-image (or path default-image-path))
        preview (core/fit source preview-max-side)]
    (view/show! preview #(pipeline/render preview (merge pipeline/default-settings %)))))
