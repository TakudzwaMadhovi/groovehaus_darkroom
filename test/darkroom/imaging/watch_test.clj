(ns darkroom.imaging.watch-test
  (:require [clojure.test :refer [deftest is]]
            [darkroom.imaging.watch :as watch])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (javax.imageio ImageIO)))

(defn- await-count [a n ms]
  (let [end (+ (System/currentTimeMillis) ms)]
    (loop [] (cond (>= (count @a) n) true (> (System/currentTimeMillis) end) false :else (do (Thread/sleep 50) (recur))))))

(deftest reports-new-images-once-written
  (let [dir (.toFile (Files/createTempDirectory "darkroom-watch" (into-array FileAttribute [])))
        got (atom [])
        stop (watch/start! dir (fn [ps] (swap! got into ps)))]
    (try
      (spit (File. dir "notes.txt") "not an image")
      (ImageIO/write (java.awt.image.BufferedImage. 6 6 java.awt.image.BufferedImage/TYPE_INT_RGB) "png" (File. dir "a.png"))
      (is (await-count got 1 8000) "the picture is reported")
      (Thread/sleep 1200)
      (is (= [(.getPath (File. dir "a.png"))] @got) "once, and the text file is ignored")
      (ImageIO/write (java.awt.image.BufferedImage. 6 6 java.awt.image.BufferedImage/TYPE_INT_RGB) "png" (File. dir "b.png"))
      (is (await-count got 2 8000) "later pictures too")
      (finally (stop)))))
