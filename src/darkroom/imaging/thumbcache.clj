(ns darkroom.imaging.thumbcache
  "On-disk thumbnail cache, so reopening a big shoot does not decode every file
  again. A thumbnail is stored as a JPEG named by a hash of the source path, its
  modification time and size, so editing or replacing a file makes a new entry.
  Cache trouble (unwritable folder, corrupt entry) is never an error: the
  thumbnail is just generated again."
  (:require [darkroom.imaging.core :as core]
            [darkroom.imaging.paths :as paths])
  (:import (java.awt.image BufferedImage)
           (java.io File)
           (java.security MessageDigest)
           (javax.imageio IIOImage ImageIO ImageWriteParam)))

(def ^:private quality 0.85)

(defn- hex [^bytes bs] (apply str (map #(format "%02x" (bit-and (long %) 0xff)) bs)))

(defn cache-file
  "The cache file for `path` thumbnails of longest side `side` under `dir`."
  ^File [^File dir path side]
  (let [^File src (File. (paths/source-file path))
        key (str (.getPath src) "|" (.lastModified src) "|" (.length src) "|" side "|" path)
        sha (.digest (MessageDigest/getInstance "SHA-1") (.getBytes key "UTF-8"))]
    (File. dir (str (hex sha) ".jpg"))))

(defn- ->buffered ^BufferedImage [{:keys [width height pixels]}]
  (let [b (BufferedImage. (int width) (int height) BufferedImage/TYPE_INT_RGB)]
    (.setRGB b 0 0 (int width) (int height) ^ints pixels 0 (int width))
    b))

(defn- write! [^File f img]
  (.mkdirs (.getParentFile f))
  (let [tmp (File/createTempFile ".thumb-" ".tmp" (.getParentFile f))
        w (.next (ImageIO/getImageWritersByFormatName "jpeg"))]
    (try
      (with-open [out (ImageIO/createImageOutputStream tmp)]
        (.setOutput w out)
        (let [p (doto (.getDefaultWriteParam w)
                  (.setCompressionMode ImageWriteParam/MODE_EXPLICIT)
                  (.setCompressionQuality (float quality)))]
          (.write w nil (IIOImage. (->buffered img) nil nil) p)))
      (java.nio.file.Files/move (.toPath tmp) (.toPath f)
                                (into-array java.nio.file.CopyOption [java.nio.file.StandardCopyOption/REPLACE_EXISTING]))
      (finally (.dispose w) (java.nio.file.Files/deleteIfExists (.toPath tmp))))))

(defn thumbnail
  "The thumbnail image map for `path`: from the cache in `dir` when present,
  otherwise `(generate path side)`, stored for next time. A frame with an
  alpha channel or odd colour model is simply regenerated on a bad entry."
  [^File dir path side generate]
  (let [f (cache-file dir path side)
        cached (when (.isFile f) (try (core/from-buffered (ImageIO/read f)) (catch Throwable _ nil)))]
    (or cached
        (let [img (generate path side)]
          (try (write! f img) (catch Throwable _ nil))
          img))))
