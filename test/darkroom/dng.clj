(ns darkroom.dng
  "Test helper: builds a tiny synthetic Bayer DNG on the fly (no binary fixtures)."
  (:import (java.io File)
           (java.nio ByteBuffer ByteOrder)))

(def ^:private srgb-from-xyz ; DNG ColorMatrix1: XYZ(D65) -> camera, camera == linear sRGB
  [[3.2406 -1.5372 -0.4986] [-0.9689 1.8758 0.0415] [0.0557 -0.2040 1.0570]])

(defn- dng-bytes
  "Minimal uncompressed 16-bit RGGB DNG whose every photosite holds `value`."
  [w h value]
  (let [entries 17
        ifd-off 8
        ifd-len (+ 2 (* 12 entries) 4)
        extra   (+ ifd-off ifd-len)          ; out-of-line data starts here
        model   "Synthetic\u0000"
        cm-off  (+ extra 16)                 ; 16 bytes for Make+Model
        an-off  (+ cm-off 72)                ; 9 SRATIONAL = 72 bytes
        data-off (+ an-off 24)               ; 3 RATIONAL = 24 bytes
        data-len (* w h 2)
        bb (doto (ByteBuffer/allocate (+ data-off data-len)) (.order ByteOrder/LITTLE_ENDIAN))
        entry (fn [tag type cnt v] (.putShort bb (unchecked-short tag)) (.putShort bb (unchecked-short type)) (.putInt bb (int cnt)) (.putInt bb (int v)))
        short-entry (fn [tag v] (.putShort bb (unchecked-short tag)) (.putShort bb (unchecked-short 3)) (.putInt bb 1) (.putShort bb (unchecked-short v)) (.putShort bb (unchecked-short 0)))]
    (.put bb (.getBytes "II")) (.putShort bb (unchecked-short 42)) (.putInt bb ifd-off)
    (.putShort bb (unchecked-short entries))
    (entry 254 4 1 0)
    (entry 256 4 1 w)
    (entry 257 4 1 h)
    (short-entry 258 16)
    (short-entry 259 1)
    (short-entry 262 32803)
    (entry 271 2 5 extra)                    ; Make "Test\0"
    (entry 272 2 10 (+ extra 5))             ; Model
    (entry 273 4 1 data-off)
    (short-entry 277 1)
    (entry 278 4 1 h)
    (entry 279 4 1 data-len)
    (.putShort bb (unchecked-short 33421)) (.putShort bb (unchecked-short 3)) (.putInt bb 2) (.putShort bb (unchecked-short 2)) (.putShort bb (unchecked-short 2)) ; CFARepeatPatternDim
    (.putShort bb (unchecked-short 33422)) (.putShort bb (unchecked-short 1)) (.putInt bb 4) (.put bb (byte-array [0 1 1 2])) ; CFAPattern RGGB
    (.putShort bb (unchecked-short 50706)) (.putShort bb (unchecked-short 1)) (.putInt bb 4) (.put bb (byte-array [1 4 0 0])) ; DNGVersion
    (entry 50717 4 1 65535)                  ; WhiteLevel
    (entry 50721 10 9 cm-off)                ; ColorMatrix1
    (entry 50728 5 3 an-off)                 ; AsShotNeutral
    (.putInt bb 0)                           ; next IFD
    (.put bb (.getBytes "Test\u0000")) (.put bb (.getBytes model))
    (.position bb cm-off)
    (doseq [row srgb-from-xyz v row] (.putInt bb (int (Math/round (* v 10000)))) (.putInt bb 10000))
    (doseq [_ (range 3)] (.putInt bb 1) (.putInt bb 1))
    (.position bb data-off)
    (dotimes [_ (* w h)] (.putShort bb (unchecked-short value)))
    (.array bb)))

(defn write-dng! ^File [w h value]
  (let [f (File/createTempFile "darkroom-" ".dng")]
    (.deleteOnExit f)
    (with-open [o (java.io.FileOutputStream. f)] (.write o (dng-bytes w h value)))
    f))

