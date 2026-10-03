(ns darkroom.imaging.raw-cli
  "RAW decoding through LibRaw's `dcraw_emu` command-line tool, for platforms where
  the Bytedeco LibRaw natives are not published (Linux ARM64). Install the tool
  with `apt install libraw-bin` (Debian/Ubuntu), `dnf install LibRaw` (Fedora) or
  `brew install libraw`; set DARKROOM_DCRAW_EMU to use a binary outside the PATH.

  Same results as the native path: the options map to the same LibRaw parameters
  (camera white balance, no auto-brighten, gamma 1.0, 16-bit). Slower, because the
  pixels travel through a pipe as a 16-bit PPM."
  (:require [darkroom.imaging.core :as core]
            [darkroom.imaging.exif :as exif]
            [darkroom.imaging.paths :as paths]
            [clojure.string :as str])
  (:import (java.io ByteArrayInputStream File)
           (java.nio ByteBuffer)
           (javax.imageio ImageIO ImageReader)
           (javax.imageio.stream ImageInputStream)))

(def ^:private search-dirs ["/usr/bin" "/usr/local/bin" "/opt/homebrew/bin" "/snap/bin"])

(defn executable
  "Path of `dcraw_emu` (DARKROOM_DCRAW_EMU, then the PATH, then common install
  directories), or nil."
  []
  (let [explicit (System/getenv "DARKROOM_DCRAW_EMU")
        dirs (concat (str/split (or (System/getenv "PATH") "") (re-pattern File/pathSeparator))
                     search-dirs)
        found (->> dirs (remove str/blank?) (map #(File. ^String % "dcraw_emu"))
                   (filter #(and (.isFile ^File %) (.canExecute ^File %))) first)]
    (cond
      (and explicit (.canExecute (File. ^String explicit))) explicit
      found (str found))))

(defn available? [] (some? (executable)))

(defn- run
  "Runs dcraw_emu with `args`; returns its stdout bytes. A failure throws ex-info
  carrying the tool's own message."
  ^bytes [args path what]
  (let [exe (or (executable)
                (throw (ex-info (str "RAW decoding needs LibRaw's dcraw_emu on this platform "
                                     "(Debian/Ubuntu: apt install libraw-bin)")
                                {:path (str path)})))
        err (File/createTempFile "dcraw-emu" ".err")]
    (try
      (let [p (-> (ProcessBuilder. ^java.util.List (into [exe] args))
                  (.redirectError err)
                  (.start))
            out (with-open [in (.getInputStream p)] (.readAllBytes in))
            rc (.waitFor p)]
        (when-not (zero? rc)
          (throw (ex-info (str what ": " (str/trim (slurp err))) {:path (str path) :exit rc})))
        out)
      (finally (.delete err)))))

(defn- parse-ppm
  "Binary 16-bit PPM (P6) -> {:width :height :channels 3 :bits 16 :data short-array}."
  [^bytes b]
  (let [n (alength b)
        ;; header: three whitespace-separated tokens after the magic, then one whitespace byte
        [magic w h maxv end]
        (loop [i 0 toks [] tok (StringBuilder.)]
          (cond
            (= 4 (count toks)) (conj toks i)
            (>= i n) (throw (ex-info "Truncated PPM from dcraw_emu" {}))
            :else (let [c (char (bit-and (aget b i) 0xFF))]
                    (if (Character/isWhitespace c)
                      (if (pos? (.length tok))
                        (recur (inc i) (conj toks (str tok)) (StringBuilder.))
                        (recur (inc i) toks tok))
                      (recur (inc i) toks (doto tok (.append c)))))))
        w (Long/parseLong w) h (Long/parseLong h)]
    (when-not (and (= "P6" magic) (= "65535" maxv))
      (throw (ex-info "Unexpected dcraw_emu output format" {:magic magic :maxval maxv})))
    (let [cnt (* w h 3)
          ^shorts data (short-array cnt)]
      (when (< (- n end) (* 2 cnt))
        (throw (ex-info "Truncated PPM from dcraw_emu" {:width w :height h})))
      (.get (.asShortBuffer (ByteBuffer/wrap b end (* 2 cnt))) data)   ; PPM samples are big-endian
      {:width (int w) :height (int h) :channels 3 :bits 16 :data data})))

(defn- args-for [{:keys [quality half-size? output-color]}]
  (cond-> ["-4" "-w" "-q" (str (or quality 3)) "-o" (str (or output-color 1))]
    half-size? (conj "-h")))

(defn- decode [path opts what]
  (parse-ppm (run (conj (args-for opts) "-Z" "-" (str path)) path what)))

(defn- as-shot-multipliers
  "Camera white-balance multipliers, normalised to the smallest. dcraw_emu does not
  report them, so they are measured: the same half-size decode with and without
  the camera white balance (`-r 1 1 1 1`) differs only by these per-channel gains,
  taken over pixels away from black and clipping."
  [path]
  (let [a (decode path {:quality 0 :half-size? true :output-color 0} "Cannot decode RAW file")
        b (parse-ppm (run ["-4" "-q" "0" "-o" "0" "-h" "-r" "1" "1" "1" "1" "-Z" "-" (str path)]
                          path "Cannot decode RAW file"))
        ^shorts da (:data a) ^shorts db (:data b)
        sa (double-array 3) sb (double-array 3)]
    (dotimes [i (quot (alength da) 3)]
      (let [j (* 3 i)
            ok (loop [c 0]
                 (or (= c 3)
                     (let [x (bit-and (aget da (+ j c)) 0xFFFF) y (bit-and (aget db (+ j c)) 0xFFFF)]
                       (and (< 2000 x 60000) (< 500 y) (recur (inc c))))))]
        (when ok
          (dotimes [c 3]
            (aset sa c (+ (aget sa c) (double (bit-and (aget da (+ j c)) 0xFFFF))))
            (aset sb c (+ (aget sb c) (double (bit-and (aget db (+ j c)) 0xFFFF))))))))
    (let [m (mapv #(if (and (pos? (aget sb %)) (pos? (aget sa %))) (/ (aget sa %) (aget sb %)) 1.0) (range 3))
          lo (reduce min m)]
      (mapv #(/ (double %) lo) m))))

(defn decode-linear
  "Same contract as darkroom.imaging.raw/decode-linear."
  [path & [{:keys [multipliers?] :as opts}]]
  (let [img (decode path opts "Cannot decode RAW file")]
    (cond-> (assoc img :color-space :linear-srgb)
      multipliers? (assoc :multipliers (as-shot-multipliers path)))))

;; ------------------------------------------------------------ embedded preview

(defn- jpeg-candidates
  "Offsets of every FF D8 FF in the file."
  [^bytes b]
  (let [n (- (alength b) 3)]
    (loop [i 0 acc (transient [])]
      (if (>= i n)
        (persistent! acc)
        (recur (inc i)
               (if (and (= -1 (aget b i)) (= -40 (aget b (inc i))) (= -1 (aget b (+ i 2))))
                 (conj! acc i)
                 acc))))))

(defn- jpeg-size
  "[w h] of the JPEG starting at `off` (header only), or nil if it is not one."
  [^bytes b off]
  (try
    (with-open [in ^ImageInputStream (ImageIO/createImageInputStream
                                      (ByteArrayInputStream. b off (- (alength b) (long off))))]
      (let [readers (ImageIO/getImageReaders in)]
        (when (.hasNext readers)
          (let [^ImageReader r (.next readers)]
            (try (.setInput r in true true)
                 [(.getWidth r 0) (.getHeight r 0)]
                 (finally (.dispose r)))))))
    (catch Throwable _ nil)))

(defn embedded-thumbnail
  "The largest JPEG embedded in the RAW file, upright, as an image map; nil if the
  longest side is below `min-side` or there is none. dcraw_emu cannot extract
  previews without writing beside the source, so the file is scanned for JPEG
  streams instead (cameras store the preview as a plain JPEG)."
  [path min-side]
  (try
    (let [f (paths/source-file path)
          ^bytes b (java.nio.file.Files/readAllBytes (.toPath (File. (str f))))
          best (->> (jpeg-candidates b)
                    (keep (fn [off] (when-let [[w h] (jpeg-size b off)] [off (max w h)])))
                    (sort-by second >)
                    first)]
      (when (and best (>= (long (second best)) (long min-side)))
        (let [off (long (first best))
              buf (ImageIO/read (ByteArrayInputStream. b off (- (alength b) off)))]
          (when buf
            (core/orient (core/from-buffered buf) (exif/orientation f))))))
    (catch Exception _ nil)))
