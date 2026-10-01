(ns darkroom.imaging.export
  "Writes an image map to disk as JPEG or PNG. Pure logic, no UI dependency."
  (:import (java.awt.image BufferedImage)
           (java.io File)
           (java.nio.file Files StandardCopyOption)
           (javax.imageio IIOImage ImageIO ImageWriteParam ImageWriter)))

(def formats
  "Supported formats. :lossy? formats take a :quality in (0, 1]."
  {:jpeg {:label "JPEG" :ext "jpg" :writer "jpeg" :lossy? true}
   :png  {:label "PNG"  :ext "png" :writer "png"  :lossy? false}})

(def default-quality 0.9)

(defn target-file
  "Resolves directory + base name + format to a File, replacing a trailing
  .jpg/.jpeg/.png the user may have typed with the format's extension."
  ^File [dir name fmt]
  (let [name (.trim (str name))
        base (.trim (str (clojure.string/replace name #"(?i)\.(jpe?g|png)$" "")))]
    (when (or (empty? base) (re-find #"[/\\]" base))
      (throw (ex-info "Invalid file name" {:name name})))
    (File. (File. (str dir)) (str base "." (:ext (formats fmt))))))

(defn- ->buffered
  "Packed-ARGB image -> BufferedImage. JPEG has no alpha channel, so for RGB
  output transparent pixels are composited over white first."
  ^BufferedImage [{:keys [width height pixels]} rgb?]
  (let [w (int width) h (int height)
        ^ints src pixels
        out (BufferedImage. w h (if rgb? BufferedImage/TYPE_INT_RGB BufferedImage/TYPE_INT_ARGB))]
    (if rgb?
      (let [^ints flat (int-array (alength src))]
        (dotimes [i (alength src)]
          (let [p (aget src i)
                a (bit-and (unsigned-bit-shift-right p 24) 0xFF)
                mix (fn ^long [^long c] (quot (+ (* c a) (* 255 (- 255 a))) 255))
                r (mix (bit-and (unsigned-bit-shift-right p 16) 0xFF))
                g (mix (bit-and (unsigned-bit-shift-right p 8) 0xFF))
                b (mix (bit-and p 0xFF))]
            (aset flat i (unchecked-int (bit-or (bit-shift-left r 16) (bit-shift-left g 8) b)))))
        (.setRGB out 0 0 w h flat 0 w))
      (.setRGB out 0 0 w h src 0 w))
    out))

(defn save!
  "Writes `img` to `dir`/`name` in `fmt` (:jpeg or :png). `quality` (0-1] only
  applies to JPEG; PNG is always lossless. The file is written to a temp file
  beside the target and moved into place, so a failure never leaves a
  half-written or clobbered file. Returns the File written."
  [img {:keys [dir name format quality] :or {quality default-quality}}]
  (let [{:keys [writer lossy?]} (or (formats format)
                                    (throw (ex-info "Unsupported format" {:format format})))
        _      (when (and lossy? (not (< 0.0 (double quality) 1.0000001)))
                 (throw (ex-info "Quality must be in (0, 1]" {:quality quality})))
        target (target-file dir name format)
        parent (.getParentFile target)
        _      (when-not (.isDirectory parent)
                 (throw (ex-info "Destination folder does not exist" {:dir (str parent)})))
        tmp    (File/createTempFile ".darkroom-" ".tmp" parent)
        ^ImageWriter w (or (.next (ImageIO/getImageWritersByFormatName ^String writer))
                           (throw (ex-info "No image writer available" {:format format})))]
    (try
      (with-open [os (ImageIO/createImageOutputStream tmp)]
        (.setOutput w os)
        (let [param (.getDefaultWriteParam w)]
          (when lossy?
            (.setCompressionMode param ImageWriteParam/MODE_EXPLICIT)
            (.setCompressionType param (first (.getCompressionTypes param)))
            (.setCompressionQuality param (float quality)))
          (.write w nil (IIOImage. (->buffered img lossy?) nil nil) param)))
      (Files/move (.toPath tmp) (.toPath target)
                  (into-array java.nio.file.CopyOption [StandardCopyOption/REPLACE_EXISTING]))
      target
      (finally
        (.dispose w)
        (Files/deleteIfExists (.toPath tmp))))))
