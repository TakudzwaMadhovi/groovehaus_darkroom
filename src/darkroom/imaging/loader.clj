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
  space, ready for the pipeline. RAW files keep their full 16-bit linear precision.
  `settings` (optional, a frame's settings): a :camera-profile (path of a .dcp)
  renders a RAW file through that profile, with :camera-profile-curve also its
  tone curve; other files ignore them."
  ([id] (load-scene id nil))
  ([id {:keys [camera-profile camera-profile-curve]}]
   (let [path (paths/source-file id)]
     (cond
       (not (raw/raw-file? path)) (scene/from-argb (load-image path))
       camera-profile (raw/load-scene-with-profile path camera-profile {:tone-curve? (boolean camera-profile-curve)})
       :else (raw/load-scene path)))))
