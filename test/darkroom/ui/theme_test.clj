(ns darkroom.ui.theme-test
  (:require [clojure.test :refer [deftest is]]
            [darkroom.ui.theme :as theme]))

(deftest tracked-text-is-announced-plain
  (doseq [level [:tight :normal :wide]
          s ["LIBRARY" "GROOVEHAUS · DARKROOM" "IMPORT +" "1:1" ""]]
    (is (= s (theme/untracked (theme/tracked s level))) (str level " " s)))
  (is (not= "LIBRARY" (theme/tracked "LIBRARY")) "tracking adds characters")
  (is (= "ON THE DECKS" (theme/untracked "O N   T H E   D E C K S"))))
