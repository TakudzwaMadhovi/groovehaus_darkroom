(ns darkroom.imaging.core
  "Pure image processing logic. No JavaFX (or any UI) dependency, so it can be
  tested headlessly and reused by other front ends.

  An image is a map {:width w :height h :pixels int-array} where pixels are
  packed ARGB ints in row-major order.")

(defn image
  "Builds an image map from dimensions and a packed-ARGB int array."
  [width height ^ints pixels]
  {:width width :height height :pixels pixels})

(defn load-image
  "Reads an image file (PNG/JPEG/GIF/BMP) from a local path into an image map."
  [path]
  (let [f   (java.io.File. (str path))
        buf (or (javax.imageio.ImageIO/read f)
                (throw (ex-info "Unreadable or unsupported image file" {:path (str path)})))
        w   (.getWidth buf)
        h   (.getHeight buf)]
    (image w h (.getRGB buf 0 0 w h nil 0 w))))

(defn- clamp-channel ^long [^long v]
  (cond (< v 0) 0 (> v 255) 255 :else v))

(defn adjust-brightness
  "Adds `amount` (-100..100, percent of full range) to each RGB channel.
  Alpha is preserved. Returns a new image; the input is not modified."
  [{:keys [width height pixels]} amount]
  (let [^ints src pixels
        n         (alength src)
        out       (int-array n)
        delta     (long (Math/round (* 255.0 (/ (double amount) 100.0))))]
    (dotimes [i n]
      (let [p (aget src i)
            a (bit-and (unsigned-bit-shift-right p 24) 0xFF)
            r (clamp-channel (+ delta (bit-and (unsigned-bit-shift-right p 16) 0xFF)))
            g (clamp-channel (+ delta (bit-and (unsigned-bit-shift-right p 8) 0xFF)))
            b (clamp-channel (+ delta (bit-and p 0xFF)))]
        (aset out i (unchecked-int (bit-or (bit-shift-left a 24)
                                           (bit-shift-left r 16)
                                           (bit-shift-left g 8)
                                           b)))))
    (image width height out)))
