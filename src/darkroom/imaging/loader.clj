(ns darkroom.imaging.loader
  "Single entry point for opening any supported image: RAW files go through
  LibRaw (which applies the camera's orientation itself), everything else
  through ImageIO plus the EXIF orientation."
  (:require [darkroom.imaging.core :as core]
            [darkroom.imaging.exif :as exif]
            [darkroom.imaging.raw :as raw]))

(defn load-image
  "Path -> image map ready for the pipeline."
  [path]
  (if (raw/raw-file? path)
    (raw/load-image path)
    (core/orient (core/load-image path) (exif/orientation path))))
