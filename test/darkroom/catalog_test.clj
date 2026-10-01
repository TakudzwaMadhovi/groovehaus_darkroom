(ns darkroom.catalog-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.catalog :as cat]
            [darkroom.imaging.pipeline :as pipeline])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(deftest names
  (is (= "IMG_0981" (cat/frame-name "/x/y/img_0981.jpg")))
  (is (= "A_VERY_LONG_NA" (cat/frame-name "/x/a_very_long_name_indeed.png")))
  (is (= 14 (count (cat/frame-name "/x/a_very_long_name_indeed.png"))))
  (is (= "DSC02150" (cat/frame-name "DSC02150.ARW")))
  (is (= "FIRST SHOOT" (cat/shoot-name "  first shoot ")))
  (is (= 28 (count (cat/shoot-name (apply str (repeat 40 "x"))))))
  (is (nil? (cat/shoot-name "   "))))

(deftest shoots
  (let [[c1 id1] (cat/add-shoot cat/empty-catalog "night" ["/a.jpg" "/b.jpg" "/a.jpg"])
        [c2 id2] (cat/add-shoot c1 "day" [])
        [c3 none] (cat/add-shoot c2 "  " [])]
    (is (= ["s1" "s2"] [id1 id2]) "ids are unique and stable")
    (is (= ["/a.jpg" "/b.jpg"] (:paths (cat/shoot c2 id1))) "duplicate paths dropped")
    (is (nil? none))
    (is (= c2 c3))
    (let [c4 (cat/add-paths c3 id1 ["/b.jpg" "/c.jpg" "/c.jpg"])]
      (is (= ["/a.jpg" "/b.jpg" "/c.jpg"] (:paths (cat/shoot c4 id1))) "append, skipping existing"))))

(deftest frames-default-and-rating
  (let [c cat/empty-catalog]
    (is (= 0 (cat/rating c "/a.jpg")))
    (is (not (cat/edited? c "/a.jpg")))
    (is (= pipeline/default-settings (cat/adj c "/a.jpg")))
    (let [c (cat/set-rating c "/a.jpg" 3)]
      (is (= 3 (cat/rating c "/a.jpg")))
      (is (= 0 (cat/rating (cat/set-rating c "/a.jpg" 3) "/a.jpg")) "same key again clears")
      (is (= 4 (cat/rating (cat/set-rating c "/a.jpg" 4) "/a.jpg"))))
    (let [c (cat/toggle-pick c "/a.jpg")]
      (is (= 5 (cat/rating c "/a.jpg")))
      (is (= ["/a.jpg"] (vec (cat/picks c ["/a.jpg" "/b.jpg"]))))
      (is (= 0 (cat/rating (cat/toggle-pick c "/a.jpg") "/a.jpg"))))))

(deftest history
  (let [p "/a.jpg"
        c (-> cat/empty-catalog (cat/set-adj p :exposure 0.5))]
    (is (cat/edited? c p))
    (is (= 1 (count (:history (cat/frame c p)))) "live changes add no history")
    (let [c (cat/commit c p "EXPOSURE")]
      (is (= ["IMPORTED" "EXPOSURE"] (map :label (:history (cat/frame c p)))))
      (is (= c (cat/commit c p "EXPOSURE")) "committing an unchanged state is a no-op")
      (let [c2 (-> c (cat/set-adj p :contrast 0.3) (cat/commit p "CONTRAST"))]
        (is (= 3 (count (:history (cat/frame c2 p)))))
        (let [back (cat/revert-to c2 p 1)]
          (is (= 0.5 (:exposure (cat/adj back p))))
          (is (= 0.0 (:contrast (cat/adj back p))))
          (is (= 3 (count (:history (cat/frame back p)))) "reverting keeps the history"))
        (is (= c2 (cat/revert-to c2 p 99)) "unknown index ignored")))
    (testing "history is capped"
      (let [big (reduce (fn [c i] (-> c (cat/set-adj p :exposure (* 0.01 i)) (cat/commit p (str "E" i))))
                        cat/empty-catalog (range 1 100))]
        (is (= (inc cat/history-limit) (count (:history (cat/frame big p)))))
        (is (= "E99" (:label (peek (:history (cat/frame big p))))))))))

(deftest presets
  (let [p "/a.jpg"
        c (-> cat/empty-catalog
              (cat/set-adj p :angle 3.5) (cat/set-adj p :aspect "4:5") (cat/set-adj p :flip true)
              (cat/set-adj p :exposure 1.0) (cat/set-adj p :denoise 40))
        c (cat/apply-preset c p "SILVER")
        a (cat/adj c p)]
    (is (= 1.0 (:bw a)))
    (is (= 0.0 (:exposure a)) "other adjustments are reset")
    (is (= 0 (:denoise a)))
    (is (= [3.5 "4:5" true] [(:angle a) (:aspect a) (:flip a)]) "geometry survives a preset")
    (is (= "SILVER" (:label (peek (:history (cat/frame c p))))))
    (let [o (cat/apply-preset c p "ORIGINAL")]
      (is (= 0.0 (:bw (cat/adj o p)))))))

(deftest persistence
  (let [dir  (.toFile (Files/createTempDirectory "darkroom-cat" (into-array FileAttribute [])))
        file (File. dir "sub/catalog.edn")
        [c id] (cat/add-shoot cat/empty-catalog "night" ["/a.jpg"])
        c (-> c (cat/set-adj "/a.jpg" :curve [0.0 0.2 0.5 0.8 1.0]) (cat/set-adj "/a.jpg" :aspect "3:2")
              (cat/set-rating "/a.jpg" 5) (cat/commit "/a.jpg" "CURVE"))]
    (cat/save! c file)
    (is (= c (cat/load! file)) "round-trips, including vectors, strings and doubles")
    (is (= ["catalog.edn"] (map #(.getName ^File %) (.listFiles (.getParentFile file)))) "no stray temp files")
    (testing "missing file -> empty"
      (is (= cat/empty-catalog (cat/load! (File. dir "nope.edn")))))
    (testing "corrupt file -> empty, original kept aside"
      (spit file "{:shoots [")
      (is (= cat/empty-catalog (cat/load! file)))
      (is (.exists (File. (str file ".bad")))))
    (testing "wrong shape -> empty"
      (spit file "[1 2 3]")
      (is (= cat/empty-catalog (cat/load! file))))))
