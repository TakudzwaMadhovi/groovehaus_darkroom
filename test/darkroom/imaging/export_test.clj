(ns darkroom.imaging.export-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.core :as core]
            [darkroom.imaging.export :as export])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

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
