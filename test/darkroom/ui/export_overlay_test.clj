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
