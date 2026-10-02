(ns darkroom.main
  "Entry point."
  (:require [darkroom.imaging.denoise :as denoise]
            [darkroom.ui.app :as app])
  (:import (java.io File))
  (:gen-class))

(defn -main
  "Optional argument: an image to open straight into Develop."
  [& [path]]
  ;; Load OpenCV natives in the background while the window opens.
  (doto (Thread. ^Runnable denoise/warm-up! "darkroom-warmup") (.setDaemon true) (.start))
  (app/show! {:file (when path (File. ^String (str path)))}))
