(ns darkroom.ui.export-overlay-test
  (:require [clojure.test :refer [deftest is]]
            [darkroom.ui.export-overlay :as overlay])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(deftest target-name-never-overwrites
  (let [d (str (.toFile (Files/createTempDirectory "darkroom-exp" (into-array FileAttribute []))))]
    (is (= "groovehaus_x" (overlay/target-name d "groovehaus_x" :jpeg)))
    (spit (File. d "groovehaus_x.jpg") "1")
    (is (= "groovehaus_x-2" (overlay/target-name d "groovehaus_x" :jpeg)))
    (spit (File. d "groovehaus_x-2.jpg") "2")
    (is (= "groovehaus_x-3" (overlay/target-name d "groovehaus_x" :jpeg)))
    (is (= "groovehaus_x" (overlay/target-name d "groovehaus_x" :png)) "other formats are independent")))

(deftest presets-and-options
  (let [s @darkroom.ui.state/state
        web (overlay/apply-export-preset s "WEB")
        print (overlay/apply-export-preset s "PRINT")
        social (overlay/apply-export-preset s "SOCIAL")]
    (is (= [:jpeg 2048 85 :srgb] ((juxt :fmt :size :q :cspace) web)))
    (is (= [:tiff 0 :adobe-rgb] ((juxt :fmt :size :cspace) print)))
    (is (= [:webp 1080 :none] [(:fmt social) (:size social) (get-in social [:export-opts :metadata])]))
    (is (= "groovehaus_{name}" (get-in web [:export-opts :template])) "unrelated options survive a preset")
    (is (= s (overlay/apply-export-preset s "NOPE")))
    (let [o (overlay/export-options (assoc (assoc-in print [:export-opts :wm-text] "(c) me") :cur nil :export-dir (File. "/x")))]
      (is (= {:for :glossy :amount :standard} (:sharpen o)))
      (is (= {:text "(c) me" :position :bottom-right :opacity 0.6} (:watermark o)))
      (is (= :tiff (:format o)))
      (is (nil? (:watermark (overlay/export-options (assoc s :export-dir (File. "/x")))))))))
