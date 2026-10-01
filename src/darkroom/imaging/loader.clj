(ns darkroom.imaging.loader
  "Single entry point for opening any supported image: RAW files go through
  LibRaw, everything else through ImageIO."
  (:require [darkroom.imaging.core :as core]
            [darkroom.imaging.raw :as raw]))

(defn load-image
  "Path -> image map ready for the pipeline."
  [path]
  (if (raw/raw-file? path)
    (raw/load-image path)
    (core/load-image path)))
