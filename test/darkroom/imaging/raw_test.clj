(ns darkroom.imaging.raw-test
  "Builds a tiny synthetic Bayer DNG on the fly (no binary fixtures) and
  checks that LibRaw decodes it to linear RGB."
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.loader :as loader]
            [darkroom.imaging.raw :as raw])
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

(defn- write-dng! ^File [w h value]
  (let [f (File/createTempFile "darkroom-" ".dng")]
    (.deleteOnExit f)
    (with-open [o (java.io.FileOutputStream. f)] (.write o (dng-bytes w h value)))
    f))

(defn- sample ^long [^shorts data ^long i] (bit-and (aget data (int i)) 0xFFFF))

(deftest recognises-raw-extensions
  (is (raw/raw-file? "a/b/IMG_0001.ARW"))
  (is (raw/raw-file? "x.dng"))
  (is (not (raw/raw-file? "photo.jpg")))
  (is (not (raw/raw-file? "dng"))))

(deftest libraw-is-loaded
  (is (re-find #"^0\.21" (raw/libraw-version))))

(deftest decodes-synthetic-dng-to-linear-rgb
  (let [f   (write-dng! 64 48 16384)         ; 25% of full scale
        img (raw/decode-linear f)
        mid (* 3 (+ (* 24 64) 32))]
    (is (= [64 48 3 16] [(:width img) (:height img) (:channels img) (:bits img)]))
    (is (= (* 64 48 3) (alength ^shorts (:data img))))
    (testing "values are scene-linear: ~25% grey stays ~25% (not gamma-encoded)"
      (doseq [c [0 1 2]]
        (is (< (Math/abs (- 16384 (sample (:data img) (+ mid c)))) 1500))))
    (testing "display conversion applies the sRGB curve: 25% linear -> ~137/255"
      (let [d (raw/linear->display img)
            p (aget ^ints (:pixels d) (+ (* 24 64) 32))]
        (is (= 255 (bit-and (unsigned-bit-shift-right p 24) 0xFF)))
        (is (< (Math/abs (- 137 (bit-and (unsigned-bit-shift-right p 8) 0xFF))) 8))))))

(deftest linear-response-doubles-with-light
  (let [lo (raw/decode-linear (write-dng! 32 32 8000))
        hi (raw/decode-linear (write-dng! 32 32 16000))
        i  (* 3 (+ (* 16 32) 16))
        ratio (/ (double (sample (:data hi) i)) (double (sample (:data lo) i)))]
    (is (< 1.9 ratio 2.1))))

(deftest loader-dispatches-and-errors-cleanly
  (let [d (loader/load-image (write-dng! 32 32 20000))]
    (is (= [32 32] [(:width d) (:height d)])))
  (is (= [800 600] (let [i (loader/load-image "resources/sample.png")] [(:width i) (:height i)])))
  (let [bad (File/createTempFile "darkroom-bad" ".arw")]
    (.deleteOnExit bad)
    (spit bad "not a raw file")
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Cannot (open|unpack)" (raw/decode-linear bad)))))
