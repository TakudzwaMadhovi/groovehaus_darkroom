(ns darkroom.imaging.loader
  "Single entry point for opening any supported image: RAW files go through
  LibRaw (which applies the camera's orientation itself), everything else
  through ImageIO plus the EXIF orientation."
  (:require [darkroom.imaging.color :as color]
            [darkroom.imaging.core :as core]
            [darkroom.imaging.exif :as exif]
            [darkroom.imaging.paths :as paths]
            [darkroom.imaging.raw :as raw]
            [darkroom.imaging.scene :as scene]))

(defn- tiff? [path] (boolean (re-find #"(?i)\.tiff?$" (str path))))

(defn float-tiff-scene
  "A 32-bit float TIFF (the scene-referred files this app writes for HDR merges
  and panoramas, or any float TIFF from other software) as a scene image, or nil
  for any other file. Values are kept as stored (above 1.0 too). Pixels are taken
  to be in the working space when the file carries this app's linear working
  profile, and linear sRGB otherwise."
  [path]
  (when (tiff? path)
    (with-open [in (javax.imageio.ImageIO/createImageInputStream (java.io.File. (str path)))]
      (let [readers (javax.imageio.ImageIO/getImageReaders in)]
        (when (.hasNext readers)
          (let [^javax.imageio.ImageReader r (.next readers)]
            (try
              (.setInput r in true true)
              (let [type (.getRawImageType r 0)]
                (when (and type (= java.awt.image.DataBuffer/TYPE_FLOAT (.getDataType (.getSampleModel type)))
                           (<= 3 (.getNumBands (.getSampleModel type))))
                  (let [img (.read r 0)
                        raster (.getRaster img)
                        w (.getWidth raster) h (.getHeight raster) bands (.getNumBands raster)
                        ^floats px (.getPixels raster 0 0 w h ^floats (float-array (* w h bands)))
                        ^floats out (float-array (* 3 w h))
                        ours? (let [cs (.getColorSpace (.getColorModel img))]
                                (and (instance? java.awt.color.ICC_ColorSpace cs)
                                     (let [^bytes got (.getData (.getProfile ^java.awt.color.ICC_ColorSpace cs))
                                           ^bytes want (color/icc-bytes :working-linear)]
                                       (and (= (alength got) (alength want))
                                            (java.util.Arrays/equals (java.util.Arrays/copyOfRange got 128 (alength got))
                                                                     (java.util.Arrays/copyOfRange want 128 (alength want)))))))
                        ^doubles m (double-array (if ours? [1 0 0 0 1 0 0 0 1] (color/convert-matrix :srgb :working)))]
                    (dotimes [i (* w h)]
                      (let [j (* i bands) k (* 3 i)
                            r (double (aget px j)) g (double (aget px (+ j 1))) b (double (aget px (+ j 2)))]
                        (aset out k (float (+ (* (aget m 0) r) (* (aget m 1) g) (* (aget m 2) b))))
                        (aset out (+ k 1) (float (+ (* (aget m 3) r) (* (aget m 4) g) (* (aget m 5) b))))
                        (aset out (+ k 2) (float (+ (* (aget m 6) r) (* (aget m 7) g) (* (aget m 8) b))))))
                    (scene/image w h out))))
              (finally (.dispose r)))))))))

(defn- scene->display
  "8-bit ARGB preview of a scene image that may exceed 1.0: scaled down by its
  99.5th percentile brightness when that is above 1 (a thumbnail of an HDR file
  would otherwise be clipped white)."
  [{:keys [width height data] :as sc}]
  (let [^floats d data n (* (long width) (long height))
        step (max 1 (quot n 20000))
        vs (sort (for [i (range 0 n step)] (double (aget d (* 3 i)))))
        p (nth vs (min (dec (count vs)) (long (* 0.995 (count vs)))))
        k (/ 1.0 (max 1.0 (double p)))]
    (scene/->argb (scene/image width height (let [out (float-array (alength d))] (dotimes [i (alength d)] (aset out i (float (* k (aget d i))))) out)))))

(defn load-image
  "Frame path (or virtual-copy id) -> 8-bit display image map (packed ARGB, sRGB)."
  [id]
  (let [path (paths/source-file id)]
    (if (raw/raw-file? path)
      (raw/load-image path)
      (if-let [sc (float-tiff-scene path)]
        (scene->display sc)
        (core/orient (core/load-image path) (exif/orientation path))))))

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
       (not (raw/raw-file? path)) (or (float-tiff-scene path) (scene/from-argb (load-image path)))
       camera-profile (raw/load-scene-with-profile path camera-profile {:tone-curve? (boolean camera-profile-curve)})
       :else (raw/load-scene path)))))
