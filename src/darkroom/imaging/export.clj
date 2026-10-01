(ns darkroom.imaging.export
  "Writes images to disk as JPEG, PNG, 16-bit TIFF or WebP, embedding an ICC
  profile (JPEG, PNG, TIFF) and, for JPEG, EXIF. WebP is written by OpenCV's
  encoder and carries no profile, so it is always sRGB. Pure logic, no UI
  dependency."
  (:require [darkroom.imaging.color :as color]
            [darkroom.imaging.exif :as exif]
            [darkroom.imaging.scene :as scene])
  (:import (java.awt.color ICC_ColorSpace ICC_Profile)
           (java.awt.image BufferedImage ComponentColorModel DataBuffer Raster)
           (java.io File)
           (java.nio.file Files StandardCopyOption)
           (java.util.zip Deflater)
           (javax.imageio IIOImage ImageIO ImageTypeSpecifier ImageWriteParam ImageWriter)
           (javax.imageio.metadata IIOMetadata IIOMetadataNode)
           (org.w3c.dom Node)))

(def formats
  "Supported formats. :lossy? formats take a :quality in (0, 1]."
  {:jpeg {:label "JPEG" :ext "jpg" :writer "jpeg" :lossy? true}
   :png  {:label "PNG"  :ext "png" :writer "png"  :lossy? false}
   :tiff {:label "TIFF 16-bit" :ext "tif" :writer "tiff" :lossy? false :scene? true}
   :webp {:label "WebP" :ext "webp" :lossy? true :scene? true :srgb-only? true}})

(def default-quality 0.9)

(defn target-file
  "Resolves directory + base name + format to a File, replacing a trailing
  .jpg/.png/.tif/.webp the user may have typed with the format's extension."
  ^File [dir name fmt]
  (let [name (.trim (str name))
        base (.trim (str (clojure.string/replace name #"(?i)\.(jpe?g|png|tiff?|webp)$" "")))]
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

(def ^:private jpeg-tree "javax_imageio_jpeg_image_1.0")
(def ^:private png-tree "javax_imageio_png_1.0")

(defn icc-app2-segments
  "JPEG APP2 payloads carrying `icc` (ICC_PROFILE chunks of at most 65519 bytes)."
  [^bytes icc]
  (let [max-chunk 65519
        n (max 1 (long (Math/ceil (/ (alength icc) (double max-chunk)))))]
    (vec (for [i (range n)]
           (let [from (* i max-chunk) to (min (alength icc) (+ from max-chunk))
                 out (java.io.ByteArrayOutputStream.)]
             (.write out (.getBytes "ICC_PROFILE" "US-ASCII")) (.write out 0)
             (.write out (int (inc i))) (.write out (int n))
             (.write out icc (int from) (int (- to from)))
             (.toByteArray out))))))

(defn- deflate ^bytes [^bytes bs]
  (let [d (Deflater.) out (java.io.ByteArrayOutputStream.) buf (byte-array 4096)]
    (.setInput d bs) (.finish d)
    (while (not (.finished d))
      (let [n (.deflate d buf)] (.write out buf 0 n)))
    (.end d)
    (.toByteArray out)))

(defn- first-element ^Node [^Node root ^String tag]
  (.item (.getElementsByTagName ^org.w3c.dom.Element root tag) 0))

(defn- marker-node ^IIOMetadataNode [tag ^bytes data]
  (doto (IIOMetadataNode. "unknown")
    (.setAttribute "MarkerTag" (str tag))
    (.setUserObject data)))

(defn- jpeg-metadata
  "Default JPEG metadata plus an EXIF APP1 and ICC APP2 segments (inserted
  ahead of the table/frame markers, after JFIF)."
  ^IIOMetadata [^ImageWriter w ^BufferedImage img param icc exif-block]
  (let [md   (.getDefaultImageMetadata w (ImageTypeSpecifier. img) param)
        root (.getAsTree md jpeg-tree)
        seq-node (first-element root "markerSequence")
        segments (concat (when exif-block [[225 exif-block]])
                         (when icc (map (fn [b] [226 b]) (icc-app2-segments icc))))]
    (doseq [[tag bs] (reverse segments)]
      (.insertBefore seq-node (marker-node tag bs) (.getFirstChild seq-node)))
    (.setFromTree md jpeg-tree root)
    md))

(defn- png-metadata
  "Default PNG metadata plus an iCCP chunk."
  ^IIOMetadata [^ImageWriter w ^BufferedImage img param ^bytes icc]
  (let [md   (.getDefaultImageMetadata w (ImageTypeSpecifier. img) param)
        root (.getAsTree md png-tree)
        node (doto (IIOMetadataNode. "iCCP")
               (.setAttribute "profileName" "ICC profile")
               (.setAttribute "compressionMethod" "deflate")
               (.setUserObject (deflate icc)))]
    (.appendChild root node)
    (.setFromTree md png-tree root)
    md))

(defn save!
  "Writes `img` (packed-ARGB image map) to `dir`/`name` in `fmt` (:jpeg or
  :png). `quality` (0-1] only applies to JPEG; PNG is always lossless.
  :icc (profile bytes) is embedded in both; :exif (an APP1 payload from
  darkroom.imaging.exif/exif-block) in JPEG only. The file is written to a temp
  file beside the target and moved into place, so a failure never leaves a
  half-written or clobbered file. Returns the File written."
  [img {:keys [dir name format quality icc exif] :or {quality default-quality}}]
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
          (let [buf (->buffered img lossy?)
                md  (cond (and lossy? (or icc exif)) (jpeg-metadata w buf param icc exif)
                          (and (not lossy?) icc)     (png-metadata w buf param icc))]
            (.write w nil (IIOImage. buf nil md) param))))
      (Files/move (.toPath tmp) (.toPath target)
                  (into-array java.nio.file.CopyOption [StandardCopyOption/REPLACE_EXISTING]))
      target
      (finally
        (.dispose w)
        (Files/deleteIfExists (.toPath tmp))))))

(defn- write-atomically!
  "Calls (write! tmp-file) for a temp file beside `target`, then moves it into
  place; a failure never leaves a half-written or clobbered file."
  [^File target write!]
  (let [parent (.getParentFile target)
        _      (when-not (.isDirectory parent)
                 (throw (ex-info "Destination folder does not exist" {:dir (str parent)})))
        tmp    (File/createTempFile ".darkroom-" ".tmp" parent)]
    (try
      (write! tmp)
      (Files/move (.toPath tmp) (.toPath target)
                  (into-array java.nio.file.CopyOption [StandardCopyOption/REPLACE_EXISTING]))
      target
      (finally (Files/deleteIfExists (.toPath tmp))))))

(defn- tiff16-image
  "BufferedImage of interleaved unsigned-16-bit RGB tagged with `icc` (so the
  TIFF writer embeds the profile)."
  ^BufferedImage [{:keys [width height data]} ^bytes icc]
  (let [w (int width) h (int height)
        cs (ICC_ColorSpace. (ICC_Profile/getInstance icc))
        cm (ComponentColorModel. cs false false java.awt.Transparency/OPAQUE DataBuffer/TYPE_USHORT)
        raster (Raster/createInterleavedRaster DataBuffer/TYPE_USHORT w h (* 3 w) 3 (int-array [0 1 2]) nil)
        ^shorts dst (.getData ^java.awt.image.DataBufferUShort (.getDataBuffer raster))]
    (System/arraycopy ^shorts data 0 dst 0 (alength ^shorts data))
    (BufferedImage. cm raster false nil)))

(defn- save-tiff16! [sc space ^File target]
  (let [enc (scene/->encoded16 sc space)
        img (tiff16-image enc (color/icc-bytes space))
        ^ImageWriter w (.next (ImageIO/getImageWritersByFormatName "tiff"))]
    (try
      (write-atomically!
        target
        (fn [tmp]
          (with-open [os (ImageIO/createImageOutputStream tmp)]
            (.setOutput w os)
            (let [param (.getDefaultWriteParam w)]
              (.setCompressionMode param ImageWriteParam/MODE_EXPLICIT)
              (.setCompressionType param "Deflate")
              (.write w nil (IIOImage. img nil nil) param)))))
      (finally (.dispose w)))))

(defn- save-webp! [sc quality ^File target]
  (let [{:keys [width height pixels]} (scene/->argb sc :srgb)
        ^ints px pixels
        n (* (long width) (long height))
        bgr (byte-array (* 3 n))]
    (dotimes [i n]
      (let [p (aget px i)]
        (aset bgr (* 3 i) (unchecked-byte p))
        (aset bgr (+ (* 3 i) 1) (unchecked-byte (unsigned-bit-shift-right p 8)))
        (aset bgr (+ (* 3 i) 2) (unchecked-byte (unsigned-bit-shift-right p 16)))))
    (write-atomically!
      target
      (fn [^File tmp]
        ;; OpenCV picks the codec from the extension, so write to a .webp name then move
        (let [named (File. (.getParentFile tmp) (str (.getName tmp) ".webp"))
              mat (org.bytedeco.opencv.opencv_core.Mat. (int height) (int width) org.bytedeco.opencv.global.opencv_core/CV_8UC3
                                                        (org.bytedeco.javacpp.BytePointer. bgr))]
          (try
            (when-not (org.bytedeco.opencv.global.opencv_imgcodecs/imwrite
                        (.getPath named) mat
                        (org.bytedeco.javacpp.IntPointer.
                          (int-array [org.bytedeco.opencv.global.opencv_imgcodecs/IMWRITE_WEBP_QUALITY
                                      (int (Math/round (* 100.0 (double quality))))])))
              (throw (ex-info "WebP encoder failed" {})))
            (Files/move (.toPath named) (.toPath tmp) (into-array java.nio.file.CopyOption [StandardCopyOption/REPLACE_EXISTING]))
            (finally (.close mat) (Files/deleteIfExists (.toPath named)))))))))

(defn save-scene!
  "Renders the float scene image `sc` into the output colour `space` (:srgb,
  :display-p3 or :adobe-rgb, default :srgb), embeds its ICC profile, and saves
  it like `save!`. Formats: :jpeg, :png, :tiff (16-bit, deflate) and :webp (always
  sRGB, no profile). For JPEG, `:tags` (darkroom.imaging.exif/read-tags of the
  source, plus any :artist / :copyright to write) become EXIF; with `:tags` nil
  no camera data is written. Returns the File written."
  [sc {:keys [space tags format quality dir name] :or {space :srgb quality default-quality} :as opts}]
  (case format
    :tiff (save-tiff16! sc space (target-file dir name :tiff))
    :webp (do (when-not (< 0.0 (double quality) 1.0000001) (throw (ex-info "Quality must be in (0, 1]" {:quality quality})))
              (save-webp! sc quality (target-file dir name :webp)))
    (save! (scene/->argb sc space)
           (assoc (dissoc opts :space :tags)
                  :icc (color/icc-bytes space)
                  :exif (exif/exif-block (or tags {}) {:srgb? (= space :srgb)})))))
