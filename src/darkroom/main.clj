(ns darkroom.main
  "Entry point: wires the image logic to the UI."
  (:require [clojure.string :as str]
            [darkroom.imaging.browser :as browser]
            [darkroom.imaging.core :as core]
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

(def thumbnail-side
  "Sidebar thumbnails are generated at 2x their 88x66 display size for Retina."
  176)

(defn- full-settings [s] (merge pipeline/default-settings s))

(defn open-session
  "Loads `file` and returns the session map the UI works on (see view/show!).
  The full-resolution `source` stays in the export closure only."
  [^File file]
  (let [file    (.getAbsoluteFile file)
        source  (loader/load-image file)
        preview (core/fit source preview-max-side)]
    {:file         file
     :preview      preview
     :render-fn    #(pipeline/render preview (full-settings %))
     :export-fn    (fn [settings opts]
                     (export/save! (pipeline/render source (full-settings settings)) opts))
     :default-name (str (str/replace (.getName file) #"\.[^.]*$" "") "-edited")
     :default-dir  (.getParentFile file)}))

(defn -main [& [path]]
  (view/show!
    {:session       (open-session (File. ^String (str (or path default-image-path))))
     :open-fn       open-session
     :analyze-fn    histogram/compute
     :scan-fn       browser/scan
     :thumbnail-fn  #(browser/thumbnail % thumbnail-side)}))
