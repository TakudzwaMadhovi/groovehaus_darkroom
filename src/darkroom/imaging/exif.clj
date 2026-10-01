(ns darkroom.imaging.exif
  "Reads EXIF metadata. Pure I/O, no UI dependency."
  (:import (com.drew.imaging ImageMetadataReader)
           (com.drew.metadata.exif ExifIFD0Directory)
           (java.io File)))

(defn orientation
  "EXIF orientation (1-8) of an image file, or 1 when absent or unreadable.
  Phones and cameras store sensor-orientation pixels plus this tag; ImageIO
  ignores it, so callers apply it with darkroom.imaging.core/orient."
  [file]
  (try
    (let [dir (.getFirstDirectoryOfType (ImageMetadataReader/readMetadata (File. (str file)))
                                        ExifIFD0Directory)]
      (if (and dir (.containsTag dir ExifIFD0Directory/TAG_ORIENTATION))
        (let [o (.getInt dir ExifIFD0Directory/TAG_ORIENTATION)]
          (if (<= 1 o 8) o 1))
        1))
    (catch Throwable _ 1)))
