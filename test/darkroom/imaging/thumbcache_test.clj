(ns darkroom.imaging.thumbcache-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.core :as core]
            [darkroom.imaging.thumbcache :as tc])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- tmp ^File [] (.toFile (Files/createTempDirectory "thumbcache" (into-array FileAttribute []))))

(defn- png! ^String [^File dir n]
  (let [f (File. dir ^String n)]
    (javax.imageio.ImageIO/write (java.awt.image.BufferedImage. 8 8 java.awt.image.BufferedImage/TYPE_INT_RGB) "png" f)
    (.getPath f)))

(defn- solid [w h argb] (core/image w h (doto (int-array (* w h)) (java.util.Arrays/fill (int argb)))))

(deftest second-request-comes-from-disk
  (let [dir (tmp) p (png! (tmp) "a.png") calls (atom 0)
        gen (fn [_ _] (swap! calls inc) (solid 8 6 (unchecked-int 0xFF3070A0)))]
    (let [a (tc/thumbnail dir p 480 gen)]
      (is (= [8 6] [(:width a) (:height a)]))
      (is (= 1 @calls)))
    (let [b (tc/thumbnail dir p 480 gen)
          px (aget ^ints (:pixels b) 0)]
      (is (= 1 @calls) "no second decode")
      (is (= [8 6] [(:width b) (:height b)]))
      (testing "JPEG round trip stays close"
        (is (< (Math/abs (- (bit-and (bit-shift-right px 16) 255) 0x30)) 6))))))

(deftest changed-file-or-size-misses
  (let [dir (tmp) src (tmp) p (png! src "a.png") calls (atom 0)
        gen (fn [_ _] (swap! calls inc) (solid 4 4 (unchecked-int 0xFF000000)))]
    (tc/thumbnail dir p 480 gen)
    (tc/thumbnail dir p 240 gen)
    (is (= 2 @calls) "size is part of the key")
    (.setLastModified (File. p) 1000000000000)
    (tc/thumbnail dir p 480 gen)
    (is (= 3 @calls) "a modified file is a new entry")))

(deftest broken-cache-is-regenerated
  (let [dir (tmp) p (png! (tmp) "a.png") calls (atom 0)
        gen (fn [_ _] (swap! calls inc) (solid 4 4 (unchecked-int 0xFF808080)))]
    (tc/thumbnail dir p 480 gen)
    (spit (tc/cache-file dir p 480) "not a jpeg")
    (is (= 4 (:width (tc/thumbnail dir p 480 gen))))
    (is (= 2 @calls))
    (testing "an unwritable cache folder is not an error"
      (let [blocker (File/createTempFile "blocker" ".x")]
        (is (= 4 (:width (tc/thumbnail (File. blocker "sub") p 480 gen))))))))
