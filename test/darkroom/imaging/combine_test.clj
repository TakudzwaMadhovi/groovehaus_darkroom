(ns darkroom.imaging.combine-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.combine :as combine]
            [darkroom.imaging.core :as core]
            [darkroom.imaging.exif :as exif]
            [darkroom.imaging.export :as export]
            [darkroom.imaging.loader :as loader]
            [darkroom.imaging.panorama-test :as pt]
            [darkroom.imaging.scene :as scene])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- tmp ^File [] (.toFile (Files/createTempDirectory "darkroom-combine" (into-array FileAttribute []))))

(defn- scaled [{:keys [width height data]} k]
  (let [^floats d data out (float-array (alength d))]
    (dotimes [i (alength d)] (aset out i (float (Math/min 1.0 (* k (aget d i))))))
    (scene/image width height out)))

(defn- scaled-up [{:keys [width height data]} k]
  (let [^floats d data out (float-array (alength d))]
    (dotimes [i (alength d)] (aset out i (float (* k (aget d i)))))
    (scene/image width height out)))

(defn- save-jpeg! [dir name sc tags]
  (export/save! (scene/->argb sc :srgb) {:dir (.getPath dir) :name name :format :jpeg :quality 0.98
                                         :exif (when tags (exif/exif-block tags {}))}))

(deftest hdr-merge-from-files
  (let [dir (tmp) truth (scaled-up (#'pt/world 240 160 3) 3.0)   ; peaks near 2.2: the middle frame clips
        fs (mapv (fn [[name k t]] (.getPath (save-jpeg! dir name (scaled truth k) {:exposure-time [1 t] :f-number [8 1] :iso 100})))
                 [["under" 0.25 800] ["mid" 1.0 200] ["over" 4.0 50]])
        progress (atom [])
        r (combine/hdr fs {:progress (fn [i n] (swap! progress conj [i n]))})]
    (is (= :exif (:source r)))
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-9) [0.25 1.0 4.0] (:factors r))))
    (is (= [[0 3] [1 3] [2 3] [3 3]] @progress))
    (is (= "under-HDR.tif" (.getName ^File (:file r))) "next to the first frame, named after it")
    (is (= dir (.getParentFile ^File (:file r))))
    (testing "the file is a scene-referred float TIFF the loader reads back, with the dynamic range of the bracket"
      (let [sc (loader/load-scene (.getPath ^File (:file r)))]
        (is (= [240 160] [(:width sc) (:height sc)]))
        (is (> (apply max (map double (:data sc))) 1.5) "highlights the middle frame clipped are back above 1")))
    (testing "a second merge does not overwrite"
      (is (= "under-HDR-2.tif" (.getName ^File (:file (combine/hdr fs))))))))

(deftest panorama-from-files
  (let [dir (tmp) wd (#'pt/world 580 240 11)
        fs (mapv (fn [[n x0 g]] (.getPath (save-jpeg! dir n (#'pt/window wd x0 0 360 240 g) nil)))
                 [["left" 0 1.0] ["right" 220 1.0]])
        r (combine/pano fs)]
    (is (= "left-Pano.tif" (.getName ^File (:file r))))
    (let [sc (loader/load-scene (.getPath ^File (:file r)))]
      (is (< 540 (:width sc) 620))
      (is (< 220 (:height sc) 260)))
    (is (= 4 (count (:valid-rect r))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (combine/pano [(.getPath (save-jpeg! dir "a" (#'pt/world 200 200 1) nil)) (.getPath (save-jpeg! dir "b" (#'pt/world 200 200 2) nil))])))))
