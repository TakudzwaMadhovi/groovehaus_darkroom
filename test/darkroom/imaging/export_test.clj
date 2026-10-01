(ns darkroom.imaging.export-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.color :as color]
            [darkroom.imaging.core :as core]
            [darkroom.imaging.exif :as exif]
            [darkroom.imaging.export :as export]
            [darkroom.imaging.loader]
            [darkroom.imaging.scene :as scene])
  (:import (java.awt.color ICC_Profile)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util.zip Inflater)
           (javax.imageio ImageIO)))

(defn- tmp-dir [] (str (Files/createTempDirectory "darkroom-test" (into-array FileAttribute []))))

(defn- noisy [w h]
  (let [r (java.util.Random. 42)]
    (core/image w h (int-array (repeatedly (* w h) #(unchecked-int (bit-or 0xFF000000 (.nextInt r 0x1000000))))))))

(deftest target-names
  (is (= "a.jpg" (.getName (export/target-file "/x" "a" :jpeg))))
  (is (= "a.png" (.getName (export/target-file "/x" "a.JPEG" :png))))
  (is (= "a.b.png" (.getName (export/target-file "/x" "a.b" :png))))
  (is (thrown? clojure.lang.ExceptionInfo (export/target-file "/x" "../evil" :png)))
  (is (thrown? clojure.lang.ExceptionInfo (export/target-file "/x" "  " :png))))

(deftest png-roundtrip-lossless
  (let [dir (tmp-dir) img (noisy 64 48)
        f (export/save! img {:dir dir :name "out" :format :png})
        back (core/load-image f)]
    (is (= "out.png" (.getName f)))
    (is (= [64 48] [(:width back) (:height back)]))
    (is (java.util.Arrays/equals ^ints (:pixels img) ^ints (:pixels back)))))

(deftest jpeg-quality-affects-size
  (let [dir (tmp-dir) img (noisy 128 128)
        lo (.length (export/save! img {:dir dir :name "lo" :format :jpeg :quality 0.1}))
        hi (.length (export/save! img {:dir dir :name "hi" :format :jpeg :quality 0.95}))]
    (is (< lo hi))
    (is (.exists (java.io.File. dir "hi.jpg")))))

(deftest jpeg-flattens-alpha-on-white
  (let [dir (tmp-dir)
        img (core/image 8 8 (int-array 64 0)) ; fully transparent
        back (core/load-image (export/save! img {:dir dir :name "t" :format :jpeg :quality 1.0}))
        p (aget ^ints (:pixels back) 0)]
    (is (every? #(> % 250) [(bit-and (unsigned-bit-shift-right p 16) 0xFF)
                            (bit-and (unsigned-bit-shift-right p 8) 0xFF)
                            (bit-and p 0xFF)]))))

(deftest safe-failures
  (let [dir (tmp-dir) img (noisy 8 8)]
    (testing "missing folder / bad quality / unknown format"
      (is (thrown? clojure.lang.ExceptionInfo (export/save! img {:dir (str dir "/nope") :name "a" :format :png})))
      (is (thrown? clojure.lang.ExceptionInfo (export/save! img {:dir dir :name "a" :format :jpeg :quality 0})))
      (is (thrown? clojure.lang.ExceptionInfo (export/save! img {:dir dir :name "a" :format :gif}))))
    (testing "no stray temp files after success or failure"
      (export/save! img {:dir dir :name "ok" :format :png})
      (is (= ["ok.png"] (map #(.getName %) (.listFiles (java.io.File. dir))))))))

;; --- colour management and metadata ------------------------------------------

(defn- jpeg-segments
  "Marker segments of a JPEG as [marker payload-bytes], up to start of scan."
  [^java.io.File f]
  (let [bs (Files/readAllBytes (.toPath f))]
    (loop [i 2 acc []]
      (let [m (bit-and (aget bs (inc i)) 0xFF)
            len (+ (bit-shift-left (bit-and (aget bs (+ i 2)) 0xFF) 8) (bit-and (aget bs (+ i 3)) 0xFF))
            acc (conj acc [m (java.util.Arrays/copyOfRange bs (+ i 4) (+ i 2 len))])]
        (if (= m 0xDA) acc (recur (+ i 2 len) acc))))))

(defn- icc-from-jpeg [f]
  (let [chunks (for [[m ^bytes p] (jpeg-segments f)
                     :when (and (= m 0xE2) (= "ICC_PROFILE" (String. p 0 11 "US-ASCII")))]
                 [(aget p 12) (java.util.Arrays/copyOfRange p 14 (alength p))])]
    (when (seq chunks)
      (let [out (java.io.ByteArrayOutputStream.)]
        (doseq [[_ ^bytes c] (sort-by first chunks)] (.write out c 0 (alength c)))
        (.toByteArray out)))))

(deftest jpeg-embeds-icc-and-exif
  (let [dir (tmp-dir)
        sc  (scene/from-argb (noisy 32 32))]
    (doseq [sp [:srgb :display-p3 :adobe-rgb]]
      (testing (str sp)
        (let [f   (export/save-scene! sc {:dir dir :name (name sp) :format :jpeg :quality 0.9 :space sp
                                          :tags {:make "ACME" :iso 200}})
              icc (icc-from-jpeg f)]
          (is (some? icc) "ICC profile present")
          (is (java.util.Arrays/equals ^bytes icc ^bytes (color/icc-bytes sp)) "and it is the profile for the space")
          (is (some? (ICC_Profile/getInstance ^bytes icc)) "which parses")
          (is (= {:make "ACME" :iso 200 :software "Groovehaus Darkroom"} (exif/read-tags f)))
          (is (some #(= 0xE1 (first %)) (jpeg-segments f)) "EXIF APP1")
          (is (= [32 32] (let [i (ImageIO/read f)] [(.getWidth i) (.getHeight i)])) "still a valid JPEG"))))))

(deftest large-icc-profiles-are-split-across-app2-segments
  (let [big (byte-array 150000 (byte 7))
        segs (export/icc-app2-segments big)]
    (is (= 3 (count segs)))
    (is (= [[1 3] [2 3] [3 3]] (map (fn [^bytes s] [(aget s 12) (aget s 13)]) segs)))
    (is (= 150000 (reduce + (map #(- (alength ^bytes %) 14) segs))))
    (is (every? #(<= (alength ^bytes %) 65535) segs))))

(deftest png-embeds-icc
  (let [dir (tmp-dir)
        sc  (scene/from-argb (noisy 16 16))
        f   (export/save-scene! sc {:dir dir :name "p3" :format :png :space :display-p3})
        bs  (Files/readAllBytes (.toPath f))
        bb  (java.nio.ByteBuffer/wrap bs)
        chunk (loop [i 8]
                (when (< (+ i 8) (alength bs))
                  (let [len (.getInt bb (int i)) type (String. bs (+ i 4) 4 "US-ASCII")]
                    (if (= type "iCCP") [i len] (recur (+ i 12 len))))))]
    (is (some? chunk) "iCCP chunk present")
    (let [[i len] chunk
          data (java.util.Arrays/copyOfRange bs (+ i 8) (+ i 8 len))
          nul  (first (keep-indexed #(when (zero? %2) %1) data))
          inf  (doto (Inflater.) (.setInput data (+ nul 2) (- (alength data) nul 2)))
          out  (byte-array 100000)
          n    (.inflate inf out)]
      (is (java.util.Arrays/equals (java.util.Arrays/copyOf out n) ^bytes (color/icc-bytes :display-p3))
          "the chunk inflates back to the Display P3 profile"))
    (is (= [16 16] (let [i (ImageIO/read f)] [(.getWidth i) (.getHeight i)])))))

(deftest save-scene-converts-into-the-output-space
  (let [dir (tmp-dir)
        red (scene/from-argb (core/image 8 8 (int-array 64 (unchecked-int 0xFFFF0000))))
        px  (fn [sp] (let [i (ImageIO/read (export/save-scene! red {:dir dir :name (str "r-" (name sp)) :format :png :space sp}))
                           p (.getRGB i 4 4)]
                       [(bit-and (unsigned-bit-shift-right p 16) 0xFF) (bit-and (unsigned-bit-shift-right p 8) 0xFF)]))]
    (is (= [255 0] (px :srgb)) "sRGB red stays (255, 0, 0)")
    (testing "sRGB red written as Display P3 is (234, 51, 35): same colour, different numbers"
      (let [[r g] (px :display-p3)]
        (is (<= 232 r 236)) (is (<= 48 g 54))))))

(deftest save-scene-keeps-the-plain-api-working
  (let [dir (tmp-dir) sc (scene/from-argb (noisy 8 8))
        f (export/save-scene! sc {:dir dir :name "d" :format :jpeg})]
    (is (= "d.jpg" (.getName f)))
    (is (thrown? clojure.lang.ExceptionInfo (export/save-scene! sc {:dir dir :name "e" :format :gif})))))

(deftest tiff16-is-sixteen-bit-and-carries-the-profile
  (let [dir (tmp-dir)
        grey (scene/image 4 4 (let [a (float-array 48)] (java.util.Arrays/fill a (float 0.5)) a))
        f (export/save-scene! grey {:dir dir :name "t" :format :tiff :space :display-p3})
        reader (.next (ImageIO/getImageReadersByFormatName "tiff"))]
    (is (= "t.tif" (.getName f)))
    (with-open [in (ImageIO/createImageInputStream f)]
      (.setInput reader in)
      (let [raster (.getRaster (.read reader 0))
            dir-md (javax.imageio.plugins.tiff.TIFFDirectory/createFromMetadata (.getImageMetadata reader 0))
            icc (.getTIFFField dir-md 34675)]
        (is (= [4 4 3] [(.getWidth raster) (.getHeight raster) (.getNumBands raster)]))
        (is (= java.awt.image.DataBuffer/TYPE_USHORT (.getDataType (.getDataBuffer raster))))
        (testing "50% linear grey is 0.7354 of full scale in sRGB / Display P3 encoding"
          (is (< (Math/abs (- (.getSample raster 1 1 0) (* 65535 0.7354))) 120)))
        (is (some? icc) "ICC profile tag present")
        (testing "the profile is ours (Java's colour engine only restamps header fields such as the CMM and platform; tags are untouched)"
          (let [^bytes got (.getAsBytes icc) ^bytes want (color/icc-bytes :display-p3)]
            (is (= (alength want) (alength got)))
            (is (java.util.Arrays/equals (java.util.Arrays/copyOfRange got 128 (alength got))
                                         (java.util.Arrays/copyOfRange want 128 (alength want))))))))
    (.dispose reader)))

(deftest webp-roundtrip
  (let [dir (tmp-dir)
        red (scene/from-argb (core/image 32 32 (int-array 1024 (unchecked-int 0xFFFF0000))))
        f (export/save-scene! red {:dir dir :name "w" :format :webp :quality 0.9})
        bs (Files/readAllBytes (.toPath f))
        m  (org.bytedeco.opencv.global.opencv_imgcodecs/imread (.getPath f))]
    (is (= "w.webp" (.getName f)))
    (is (= ["RIFF" "WEBP"] [(String. bs 0 4 "US-ASCII") (String. bs 8 4 "US-ASCII")]))
    (is (= [32 32] [(.cols m) (.rows m)]))
    (let [b (.createIndexer m) ; BGR
          at (fn [c] (.get ^org.bytedeco.javacpp.indexer.UByteIndexer b 16 16 c))]
      (is (> (at 2) 230) "red channel") (is (< (at 1) 40) "green channel"))
    (testing "no temporary file is left behind"
      (is (= ["w.webp"] (mapv #(.getName %) (.listFiles (java.io.File. dir))))))
    (is (thrown? clojure.lang.ExceptionInfo (export/save-scene! red {:dir dir :name "q" :format :webp :quality 2.0})))))

(deftest float-tiff-keeps-everything-and-round-trips-through-the-loader
  (let [dir (tmp-dir)
        w 5 h 4
        a (float-array (* 3 w h))
        _ (dotimes [i (* w h)] (aset a (* 3 i) (float (* 0.5 i))) (aset a (+ (* 3 i) 1) (float 0.25)) (aset a (+ (* 3 i) 2) (float (- (* 0.1 i) 0.5))))
        sc (scene/image w h a)
        f (export/save-scene! sc {:dir dir :name "hdr" :format :tiff32})
        back (darkroom.imaging.loader/load-scene (.getPath f))]
    (is (= "hdr.tif" (.getName f)))
    (is (= [w h] [(:width back) (:height back)]))
    (testing "bit-exact floats: values above 1 and below 0 survive"
      (is (java.util.Arrays/equals ^floats a ^floats (:data back))))
    (testing "the file carries the linear working profile"
      (let [r (.next (ImageIO/getImageReadersByFormatName "tiff"))]
        (with-open [in (ImageIO/createImageInputStream f)]
          (.setInput r in)
          (let [cs (.getColorSpace (.getColorModel (.read r 0)))]
            (is (instance? java.awt.color.ICC_ColorSpace cs))))
        (.dispose r)))
    (testing "a preview of an over-range file is not clipped to white"
      (let [d (darkroom.imaging.loader/load-image (.getPath f))
            p (aget ^ints (:pixels d) 9)]
        (is (< (bit-and (unsigned-bit-shift-right p 16) 0xFF) 255))))
    (testing "ordinary TIFFs are not mistaken for scene files"
      (let [t (export/save-scene! sc {:dir dir :name "plain" :format :tiff})]
        (is (nil? (darkroom.imaging.loader/float-tiff-scene (.getPath t))))))
    (testing "a float TIFF from other software (linear sRGB, no profile of ours) is converted to the working space"
      ;; written as plain float sRGB-tagged data
      (let [cs (java.awt.color.ColorSpace/getInstance java.awt.color.ColorSpace/CS_LINEAR_RGB)
            cm (java.awt.image.ComponentColorModel. cs false false java.awt.Transparency/OPAQUE java.awt.image.DataBuffer/TYPE_FLOAT)
            sm (java.awt.image.PixelInterleavedSampleModel. java.awt.image.DataBuffer/TYPE_FLOAT 2 2 3 6 (int-array [0 1 2]))
            raster (java.awt.image.Raster/createWritableRaster sm nil)
            _ (.setPixels raster 0 0 2 2 (float-array (mapcat identity (repeat 4 [1.0 0.0 0.0]))))
            img (java.awt.image.BufferedImage. cm raster false nil)
            ext (java.io.File. ^String dir "ext.tif")]
        (ImageIO/write img "tiff" ext)
        (let [sc2 (darkroom.imaging.loader/float-tiff-scene (.getPath ext))
              want (color/mat-vec (color/convert-matrix :srgb :working) [1 0 0])]
          (is (some? sc2))
          (is (every? true? (map #(< (Math/abs (- (double %1) (double %2))) 1e-3) want (take 3 (:data sc2))))))))))
