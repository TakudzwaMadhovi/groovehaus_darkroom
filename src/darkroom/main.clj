(ns darkroom.main
  "Entry point: wires the image logic to the UI."
  (:require [darkroom.imaging.core :as core]
            [darkroom.imaging.export :as export]
            [darkroom.imaging.histogram :as histogram]
            [darkroom.imaging.loader :as loader]
            [darkroom.imaging.pipeline :as pipeline]
            [darkroom.ui.view :as view])
  (:import (java.io File))
  (:gen-class))

(def default-image-path "resources/sample.png")

(def preview-max-side
  "Longest side of the on-screen proxy. Sliders re-render this; export
  re-renders the full-resolution original."
  1600)

(defn- full-settings [s] (merge pipeline/default-settings s))

(defn -main [& [path]]
  (let [path    (or path default-image-path)
        file    (.getAbsoluteFile (File. ^String path))
        source  (loader/load-image file)
        preview (core/fit source preview-max-side)]
    (view/show!
      {:preview      preview
       :render-fn    #(pipeline/render preview (full-settings %))
       :analyze-fn   histogram/compute
       :export-fn    (fn [settings opts]
                       (export/save! (pipeline/render source (full-settings settings)) opts))
       :default-name (str (clojure.string/replace (.getName file) #"\.[^.]*$" "") "-edited")
       :default-dir  (.getParentFile file)})))
