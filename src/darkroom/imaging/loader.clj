(ns darkroom.imaging.loader
  "Single entry point for opening any supported image: RAW files go through
  LibRaw (which applies the camera's orientation itself), everything else
  through ImageIO plus the EXIF orientation."
  (:require [darkroom.imaging.core :as core]
            [darkroom.imaging.exif :as exif]
            [darkroom.imaging.paths :as paths]
            [darkroom.imaging.raw :as raw]
            [darkroom.imaging.scene :as scene]))

(defn load-image
  "Frame path (or virtual-copy id) -> 8-bit display image map (packed ARGB, sRGB)."
  [id]
  (let [path (paths/source-file id)]
    (if (raw/raw-file? path)
      (raw/load-image path)
      (core/orient (core/load-image path) (exif/orientation path)))))

(defn load-scene
  "Frame path (or virtual-copy id) -> float scene image in the working colour
  space, ready for the pipeline. RAW files keep their full 16-bit linear precision."
  [id]
  (let [path (paths/source-file id)]
    (if (raw/raw-file? path)
      (raw/load-scene path)
      (scene/from-argb (load-image path)))))
