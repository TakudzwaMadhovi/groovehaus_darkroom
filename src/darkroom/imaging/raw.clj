(ns darkroom.imaging.raw
  "Decodes camera RAW files (DNG, Sony ARW, Canon CR2/CR3, Nikon NEF, ...) with
  LibRaw (via the Bytedeco JavaCPP preset) into linear RGB. Pure logic, no UI
  dependency.

  A linear image is {:width w :height h :channels 3 :bits 16 :color-space
  :linear-srgb :data short-array}. `data` is interleaved RGB; each sample is an
  unsigned 16-bit value (read it with (bit-and s 0xFFFF)) proportional to
  scene light: gamma 1.0, sRGB/Rec. 709 primaries, camera white balance
  applied, no automatic brightening."
  (:require [darkroom.imaging.camera :as camera]
            [darkroom.imaging.core :as core]
            [darkroom.imaging.scene :as scene])
  (:import (java.io ByteArrayInputStream)
           (java.nio ByteOrder)
           (javax.imageio ImageIO)
           (org.bytedeco.javacpp BytePointer)
           (org.bytedeco.libraw libraw_colordata_t libraw_data_t libraw_output_params_t libraw_processed_image_t)
           (org.bytedeco.libraw.global LibRaw)))

(set! *unchecked-math* :warn-on-boxed)

(def raw-extensions
  #{"dng" "arw" "sr2" "srf" "cr2" "cr3" "crw" "nef" "nrw" "orf" "raf" "rw2"
    "rwl" "pef" "srw" "3fr" "dcr" "kdc" "mrw" "x3f" "erf" "mef" "mos" "iiq" "raw"})

(defn raw-file?
  "True if the path's extension is a known RAW format."
  [path]
  (let [n (.toLowerCase (str path))
        i (.lastIndexOf n ".")]
    (and (pos? i) (contains? raw-extensions (subs n (inc i))))))

(defn libraw-version [] (.getString (LibRaw/libraw_version)))

(defn- check! [^long rc what path]
  (when-not (zero? rc)
    (throw (ex-info (str what ": " (.getString (LibRaw/libraw_strerror (int rc))))
                    {:path (str path) :libraw-code rc}))))

(defn- configure! [^libraw_output_params_t p {:keys [quality half-size? output-color]}]
  (doto p
    (.gamm 0 1.0)               ; gamma 1.0 = linear light
    (.gamm 1 1.0)               ; no toe slope
    (.output_bps 16)
    (.output_color (int (or output-color 1))) ; 1 = sRGB / Rec. 709 primaries
    (.no_auto_bright 1)         ; keep scene-referred values
    (.use_camera_wb 1)
    (.user_qual (int (or quality 3))) ; 3 = AHD demosaic
    (.half_size (if half-size? 1 0))))

(defn decode-linear
  "Decodes the RAW file at `path` into a linear image (see ns docstring).
  Options: :quality  demosaic algorithm 0-12 (0 linear, 2 PPG, 3 AHD default),
           :half-size? true for a 2x-downsampled, much faster decode.
  Throws ex-info with LibRaw's message for unreadable/unsupported files."
  [path & [opts]]
  (let [^libraw_data_t lr (or (LibRaw/libraw_init 0)
                              (throw (ex-info "LibRaw init failed" {})))]
    (try
      (configure! (.params lr) opts)
      (check! (LibRaw/libraw_open_file lr (str path)) "Cannot open RAW file" path)
      (check! (LibRaw/libraw_unpack lr) "Cannot unpack RAW data" path)
      (check! (LibRaw/libraw_dcraw_process lr) "Cannot process RAW data" path)
      (let [err (int-array 1)
            ^libraw_processed_image_t img (LibRaw/libraw_dcraw_make_mem_image lr err)]
        (check! (aget err 0) "Cannot build image" path)
        (try
          (when-not (and (= 3 (.colors img)) (= 16 (.bits img)))
            (throw (ex-info "Unexpected LibRaw output format"
                            {:colors (.colors img) :bits (.bits img)})))
          ;; width/height are C uint16 exposed as Java short: mask them.
          (let [w (bit-and (.width img) 0xFFFF)
                h (bit-and (.height img) 0xFFFF)
                n (* w h 3)
                ^shorts data (short-array n)]
            (.get (.asShortBuffer (.order (.asByteBuffer (.limit ^BytePointer (.data img) (* 2 n)))
                                          (ByteOrder/nativeOrder)))
                  data)
            (cond-> {:width w :height h :channels 3 :bits 16
                     :color-space :linear-srgb :data data}
              (:multipliers? opts) (assoc :multipliers (let [^libraw_colordata_t c (.color lr)
                                                             pm (mapv #(double (.pre_mul c (int %))) (range 3))
                                                             lo (reduce min (filter pos? pm))]
                                                         (mapv #(/ (double %) (double lo)) pm)))))
          (finally (LibRaw/libraw_dcraw_clear_mem img))))
      (finally (LibRaw/libraw_close lr)))))

(defn- srgb-lut
  "65536-entry table: 16-bit linear value -> 8-bit sRGB-encoded value."
  []
  (let [lut (int-array 65536)]
    (dotimes [i 65536]
      (let [l (/ (double i) 65535.0)
            e (if (<= l 0.0031308) (* 12.92 l) (- (* 1.055 (Math/pow l (/ 1.0 2.4))) 0.055))]
        (aset lut i (int (Math/round (* 255.0 e))))))
    lut))

(def ^:private ^"[I" lut (srgb-lut))

(defn linear->display
  "Converts a linear image to the 8-bit sRGB-encoded ARGB image map the rest of
  the app edits (see darkroom.imaging.core)."
  [{:keys [width height data]}]
  (let [^shorts src data
        n (* (long width) (long height))
        ^ints out (int-array n)]
    (dotimes [i n]
      (let [j (* 3 i)
            r (aget lut (bit-and (aget src j) 0xFFFF))
            g (aget lut (bit-and (aget src (+ j 1)) 0xFFFF))
            b (aget lut (bit-and (aget src (+ j 2)) 0xFFFF))]
        (aset out i (unchecked-int (bit-or 0xFF000000 (bit-shift-left r 16) (bit-shift-left g 8) b)))))
    {:width width :height height :pixels out}))

(defn load-scene
  "RAW file -> float scene image in the working colour space (LibRaw output
  colour 4, 16-bit linear, camera white balance). This is what the editor
  works on; nothing is quantised to 8 bits."
  [path & [opts]]
  (scene/from-linear16 (decode-linear path (assoc opts :output-color 4))))

(defn load-scene-with-profile
  "RAW file -> float scene image in the working colour space, with the colour
  rendering of the camera profile (.dcp) at `profile-path` instead of LibRaw's
  built-in matrix: the camera's own RGB (as-shot white balance applied) goes
  through darkroom.imaging.camera. `opts` :tone-curve? applies the profile's tone
  curve. Needs a three-colour sensor."
  [path profile-path & [opts]]
  (let [{:keys [multipliers] :as img} (decode-linear path (assoc opts :output-color 0 :multipliers? true))
        [mr mg mb] (map double multipliers)
        neutral [(/ (double mg) (double mr)) 1.0 (/ (double mg) (double mb))]]
    (camera/render (scene/from-linear16 img) (camera/load-profile profile-path) neutral opts)))

(defn load-image
  "RAW file -> display image map (decode, then sRGB-encode)."
  [path & [opts]]
  (linear->display (decode-linear path opts)))

(def ^:private libraw-flip->exif
  "LibRaw's `flip` (0 none, 3 = 180, 5 = 90 CCW, 6 = 90 CW) as an EXIF orientation."
  {3 3, 5 8, 6 6})

(defn embedded-thumbnail
  "The camera's embedded JPEG preview, upright, as an image map; nil if the
  file has none, it is not a JPEG, or its longest side is below `min-side`.
  Far faster than a full decode: no demosaicing. Useful for thumbnails."
  [path min-side]
  (let [^libraw_data_t lr (or (LibRaw/libraw_init 0) (throw (ex-info "LibRaw init failed" {})))]
    (try
      (when (and (zero? (LibRaw/libraw_open_file lr (str path)))
                 (zero? (LibRaw/libraw_unpack_thumb lr)))
        (let [t   (.thumbnail lr)
              len (.tlength t)]
          ;; Identify JPEG by its magic bytes (FF D8) rather than `tformat`: the
          ;; enum read through JavaCPP was unreliable, and ImageIO needs a JPEG anyway.
          (when (> len 2)
            (let [bytes (byte-array len)
                  ^BytePointer p (.thumb t)]
              (.get (.capacity p len) bytes)
              (when-let [buf (when (and (= -1 (aget bytes 0)) (= -40 (aget bytes 1)))
                               (ImageIO/read (ByteArrayInputStream. bytes)))]
                (let [img (core/orient (core/from-buffered buf)
                                       (libraw-flip->exif (.flip (.sizes lr))))]
                  (when (>= (long (max (long (:width img)) (long (:height img)))) (long min-side))
                    img)))))))
      (catch Exception _ nil)
      (finally (LibRaw/libraw_close lr)))))
