(ns darkroom.imaging.export-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.color :as color]
            [darkroom.imaging.core :as core]
            [darkroom.imaging.exif :as exif]
            [darkroom.imaging.export :as export]
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
