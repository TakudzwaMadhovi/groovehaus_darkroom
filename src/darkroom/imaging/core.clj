(ns darkroom.imaging.core
  "Pure image processing logic. No JavaFX (or any UI) dependency, so it can be
  tested headlessly and reused by other front ends.

  An image is a map {:width w :height h :pixels int-array} where pixels are
  packed ARGB ints in row-major order. Every operation returns a new image and
  leaves its input untouched.")

(set! *unchecked-math* :warn-on-boxed)

(defn image
  "Builds an image map from dimensions and a packed-ARGB int array."
  [width height ^ints pixels]
  {:width width :height height :pixels pixels})

(defn from-buffered
  "BufferedImage -> image map (packed ARGB)."
  [^java.awt.image.BufferedImage buf]
  (let [w (.getWidth buf)
        h (.getHeight buf)]
    (image w h (.getRGB buf 0 0 w h nil 0 w))))

(defn load-image
  "Reads an image file (PNG/JPEG/GIF/BMP) from a local path into an image map."
  [path]
  (let [f (java.io.File. (str path))]
    (from-buffered
      (or (javax.imageio.ImageIO/read f)
          (throw (ex-info "Unreadable or unsupported image file" {:path (str path)}))))))

;; ---------------------------------------------------------------- helpers

(defmacro ^:private alpha [p] `(bit-and ~p 0xFF000000))
(defmacro ^:private red   [p] `(bit-and (unsigned-bit-shift-right ~p 16) 0xFF))
(defmacro ^:private green [p] `(bit-and (unsigned-bit-shift-right ~p 8) 0xFF))
(defmacro ^:private blue  [p] `(bit-and ~p 0xFF))

(defn- clamp ^long [^long v]
  (cond (< v 0) 0 (> v 255) 255 :else v))

(defn- pack ^long [^long a ^long r ^long g ^long b]
  (bit-or a (bit-shift-left r 16) (bit-shift-left g 8) b))

(defn- parallel-ranges!
  "Calls (f start end) over [0,n) split across the available cores, blocking
  until all finish. Small inputs run inline."
  [^long n f]
  (let [cores (.availableProcessors (Runtime/getRuntime))]
    (if (or (< cores 2) (< n 200000))
      (f 0 n)
      (let [chunk (long (Math/ceil (/ (double n) cores)))]
        (->> (range 0 n chunk)
             (mapv (fn [s] (future (f s (min n (+ (long s) chunk))))))
             (run! deref))))))

(defn- map-pixels
  "New image from `img` where each pixel is (f packed-argb) -> packed-argb."
  [{:keys [width height pixels]} f]
  (let [^ints src pixels
        ^ints out (int-array (alength src))]
    (parallel-ranges!
      (alength src)
      (fn [^long start ^long end]
        (loop [i start]
          (when (< i end)
            (aset out i (unchecked-int (f (aget src i))))
            (recur (inc i))))))
    (image width height out)))

(defn- map-channels
  "Applies a 256-entry lookup table to the R, G and B channels (alpha kept)."
  [img ^ints lut]
  (map-pixels img
              (fn [^long p]
                (pack (alpha p)
                      (aget lut (red p)) (aget lut (green p)) (aget lut (blue p))))))

(defn- build-lut ^ints [f]
  (let [lut (int-array 256)]
    (dotimes [v 256] (aset lut v (int (clamp (Math/round (double (f v)))))))
    lut))

;; ------------------------------------------------------------ operations

(defn fit
  "Returns `img` shrunk (box-averaged, integer factor) so its longest side is
  at most `max-side`. Returns `img` itself if it already fits. Use for fast
  interactive previews; keep the original for full-quality export."
  [{:keys [^long width ^long height pixels] :as img} max-side]
  (let [k (long (Math/ceil (/ (double (max width height)) (double max-side))))]
    (if (<= k 1)
      img
      (let [^ints src pixels
            w    (quot width k)
            h    (quot height k)
            ^ints out (int-array (* w h))
            area (* k k)]
        (dotimes [y h]
          (dotimes [x w]
            (let [x0 (* x k) y0 (* y k)]
              (loop [j 0 a 0 r 0 g 0 b 0]
                (if (< j k)
                  (let [row (+ (* (+ y0 j) width) x0)
                        [a r g b] (loop [i 0 a a r r g g b b]
                                    (if (< i k)
                                      (let [p (aget src (+ row i))]
                                        (recur (inc i) (+ a (bit-and (unsigned-bit-shift-right p 24) 0xFF))
                                               (+ r (red p)) (+ g (green p)) (+ b (blue p))))
                                      [a r g b]))]
                    (recur (inc j) (long a) (long r) (long g) (long b)))
                  (aset out (+ (* y w) x)
                        (unchecked-int (pack (bit-shift-left (quot a area) 24)
                                             (quot r area) (quot g area) (quot b area)))))))))
        (image w h out)))))

(defn adjust-brightness
  "Adds `amount` (-100..100, percent of full range) to each RGB channel."
  [img amount]
  (let [delta (Math/round (* 255.0 (/ (double amount) 100.0)))]
    (map-channels img (build-lut #(+ (long %) delta)))))

(defn adjust-contrast
  "Scales distance from mid-grey. `amount` -100 (flat) .. 100 (high)."
  [img amount]
  (let [c (* 2.55 (double amount))
        f (/ (* 259.0 (+ c 255.0)) (* 255.0 (- 259.0 c)))]
    (map-channels img (build-lut #(+ 128.0 (* f (- (double %) 128.0)))))))

(defn adjust-gamma
  "Power-law midtone curve. `gamma` > 1 brightens midtones, < 1 darkens;
  1.0 is neutral. Black and white points are unchanged."
  [img gamma]
  (let [inv (/ 1.0 (double gamma))]
    (map-channels img (build-lut #(* 255.0 (Math/pow (/ (double %) 255.0) inv))))))

(defn adjust-saturation
  "Blends each pixel with its luma (Rec. 709). `amount` -100 (greyscale) ..
  100 (double saturation); 0 is neutral."
  [img amount]
  (let [s (long (Math/round (* 256.0 (+ 1.0 (/ (double amount) 100.0)))))]
    (map-pixels img
                (fn [^long p]
                  (let [r (red p) g (green p) b (blue p)
                        y (bit-shift-right (+ (* 54 r) (* 183 g) (* 19 b)) 8)
                        mix (fn ^long [^long c] (clamp (+ y (bit-shift-right (* (- c y) s) 8))))]
                    (pack (alpha p) (mix r) (mix g) (mix b)))))))

(defn orient
  "Rotates/mirrors `img` according to an EXIF orientation value (1-8) so it
  displays upright. 1 (or anything unknown) returns `img` unchanged.
    2 mirror horizontally  3 rotate 180      4 mirror vertically
    5 transpose            6 rotate 90 CW    7 transverse     8 rotate 90 CCW"
  [{:keys [^long width ^long height pixels] :as img} orientation]
  (let [o (long (or orientation 1))]
    (if-not (<= 2 o 8)
      img
      (let [^ints src pixels
            swap? (>= o 5)
            dw (if swap? height width)
            dh (if swap? width height)
            ^ints out (int-array (* dw dh))]
        (dotimes [y dh]
          (dotimes [x dw]
            (let [sx (case o 2 (- width 1 x), 3 (- width 1 x), 4 x,
                       5 y, 6 y, 7 (- width 1 y), 8 (- width 1 y))
                  sy (case o 2 y, 3 (- height 1 y), 4 (- height 1 y),
                       5 x, 6 (- height 1 x), 7 (- height 1 x), 8 x)]
              (aset out (+ (* y dw) x) (aget src (+ (* sy width) sx))))))
        (image dw dh out)))))
