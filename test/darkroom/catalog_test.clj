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

;; --- undo / redo -----------------------------------------------------------------------

(defn- edit [c p k v label] (-> c (cat/set-adj p k v) (cat/commit p label)))

(deftest undo-and-redo
  (let [p "/a.jpg"
        c (-> cat/empty-catalog (edit p :exposure 0.5 "E1") (edit p :contrast 0.3 "C1") (edit p :saturation 0.2 "S1"))]
    (is (= ["IMPORTED" "E1" "C1" "S1"] (map :label (:history (cat/frame c p)))))
    (is (cat/can-undo? c p)) (is (not (cat/can-redo? c p)))
    (let [u1 (cat/undo c p)]
      (is (= [0.5 0.3 0.0] ((juxt :exposure :contrast :saturation) (cat/adj u1 p))))
      (is (cat/can-redo? u1 p))
      (is (= 4 (count (:history (cat/frame u1 p)))) "undo keeps the steps")
      (let [u2 (cat/undo (cat/undo u1 p) p)]
        (is (= [0.0 0.0 0.0] ((juxt :exposure :contrast :saturation) (cat/adj u2 p))))
        (is (not (cat/can-undo? u2 p)))
        (is (= (cat/adj u2 p) (cat/adj (cat/undo u2 p) p)) "undo at the start is a no-op")
        (is (= [0.5 0.0 0.0] ((juxt :exposure :contrast :saturation) (cat/adj (cat/redo u2 p) p))) "redo walks forward again")
        (is (= [0.5 0.3 0.0] ((juxt :exposure :contrast :saturation) (cat/adj (cat/redo (cat/redo u2 p) p) p)))))
      (testing "redo at the end changes nothing"
        (is (= c (cat/redo c p))))
      (testing "a new edit after an undo discards the undone steps"
        (let [d (edit u1 p :grain 0.4 "G1")]
          (is (= ["IMPORTED" "E1" "C1" "G1"] (map :label (:history (cat/frame d p)))))
          (is (not (cat/can-redo? d p)))
          (is (= 0.0 (:saturation (cat/adj d p)))))))
    (testing "undo throws away uncommitted live edits first, before stepping back"
      (let [live (cat/set-adj c p :exposure 1.5)]
        (is (= [0.5 0.2] ((juxt :exposure :saturation) (cat/adj (cat/undo live p) p))) "back to the committed state")
        (is (= 0.0 (:saturation (cat/adj (cat/undo (cat/undo live p) p) p))) "then a real step back")))
    (testing "reverting by index moves the position and keeps redo available"
      (let [r (cat/revert-to c p 1)]
        (is (= 0.5 (:exposure (cat/adj r p)))) (is (cat/can-redo? r p))
        (is (= 0.3 (:contrast (cat/adj (cat/redo r p) p))))))
    (testing "catalogs saved before undo existed (no :hpos) behave as if at the last step"
      (let [old (update-in c [:frames p] dissoc :hpos)]
        (is (not (cat/can-redo? old p))) (is (cat/can-undo? old p))
        (is (= 0.3 (:contrast (cat/adj (cat/undo old p) p))))))))

;; --- flags, labels, keywords, notes ------------------------------------------------------

(deftest reject-flag
  (let [c (cat/toggle-reject cat/empty-catalog "/a.jpg")]
    (is (cat/rejected? c "/a.jpg")) (is (not (cat/rejected? c "/b.jpg")))
    (is (not (cat/rejected? (cat/toggle-reject c "/a.jpg") "/a.jpg")))))

(deftest colour-labels
  (let [c (cat/set-colour cat/empty-catalog "/a.jpg" :red)]
    (is (= :red (cat/colour c "/a.jpg")))
    (is (= :blue (cat/colour (cat/set-colour c "/a.jpg" :blue) "/a.jpg")))
    (is (nil? (cat/colour (cat/set-colour c "/a.jpg" :red) "/a.jpg")) "the same colour clears")
    (is (nil? (cat/colour (cat/set-colour c "/a.jpg" nil) "/a.jpg")))))

(deftest keywords-and-notes
  (is (= ["night" "dj" "crowd"] (cat/parse-keywords " Night, DJ ,crowd,, night ")))
  (let [p "/a.jpg"
        c (-> cat/empty-catalog (cat/add-keywords p "night, crowd") (cat/add-keywords p "dj, Night"))]
    (is (= ["crowd" "dj" "night"] (cat/keywords c p)) "sorted, no duplicates")
    (is (= ["crowd" "night"] (cat/keywords (cat/remove-keyword c p "DJ") p)))
    (is (= [] (cat/keywords c "/b.jpg"))))
  (let [c (-> cat/empty-catalog (cat/set-meta "/a.jpg" :title " Opening night ") (cat/set-meta "/a.jpg" :copyright "(c) Me"))]
    (is (= {:title "Opening night" :copyright "(c) Me"} (cat/frame-meta c "/a.jpg")))
    (is (= {:copyright "(c) Me"} (cat/frame-meta (cat/set-meta c "/a.jpg" :title "  ") "/a.jpg")) "blank clears")
    (is (thrown? AssertionError (cat/set-meta c "/a.jpg" :nonsense "x")))))

;; --- snapshots ---------------------------------------------------------------------------

(deftest snapshots
  (let [p "/a.jpg"
        c (-> cat/empty-catalog (edit p :exposure 0.5 "E") (cat/add-snapshot p " warm look "))
        c (-> c (edit p :exposure -1.0 "E2") (cat/add-snapshot p "cold"))]
    (is (= ["WARM LOOK" "COLD"] (map :name (cat/snapshots c p))))
    (let [r (cat/apply-snapshot c p 0)]
      (is (= 0.5 (:exposure (cat/adj r p))))
      (is (= "SNAPSHOT WARM LOOK" (:label (peek (:history (cat/frame r p))))) "applying a snapshot is an undoable step")
      (is (= -1.0 (:exposure (cat/adj (cat/undo r p) p)))))
    (is (= ["COLD"] (map :name (cat/snapshots (cat/delete-snapshot c p 0) p))))
    (is (= c (cat/apply-snapshot c p 9)) "unknown index ignored")))

;; --- user presets -------------------------------------------------------------------------

(deftest user-presets
  (let [p "/a.jpg" q "/b.jpg"
        c (-> cat/empty-catalog (cat/set-adj p :exposure 0.7) (cat/set-adj p :bw 1.0) (cat/set-adj p :crop [0.1 0.1 0.5 0.5])
              (cat/set-adj p :spots [{:x 0.5 :y 0.5 :r 0.02 :mode :heal}]) (cat/set-adj p :angle 2.0)
              (cat/commit p "EDIT") (cat/save-preset p "my look"))]
    (is (= [["MY LOOK" {}]] (map (fn [[n a]] [n (select-keys a [])]) (cat/user-presets c))))
    (let [[_ saved] (first (cat/user-presets c))]
      (is (= 0.7 (:exposure saved))) (is (= 1.0 (:bw saved)))
      (is (not-any? #(contains? saved %) cat/preserved-keys) "geometry, spots and masks stay out of a preset"))
    (testing "applying it elsewhere keeps that frame's own geometry"
      (let [c2 (-> c (cat/set-adj q :angle -4.0) (cat/set-adj q :exposure -1.0) (cat/apply-user-preset q "MY LOOK"))
            a (cat/adj c2 q)]
        (is (= 0.7 (:exposure a))) (is (= 1.0 (:bw a))) (is (= -4.0 (:angle a))) (is (nil? (:crop a)))
        (is (= "MY LOOK" (:label (peek (:history (cat/frame c2 q))))))))
    (testing "saving under an existing name replaces it"
      (let [c3 (-> c (cat/set-adj p :exposure 0.1) (cat/save-preset p "MY LOOK"))]
        (is (= 1 (count (cat/user-presets c3)))) (is (= 0.1 (:exposure (second (first (cat/user-presets c3))))))))
    (testing "delete"
      (is (empty? (cat/user-presets (cat/delete-preset c "MY LOOK")))))
    (testing "a blank name saves nothing"
      (is (= c (cat/save-preset c p "   "))))
    (testing "export and import round-trip"
      (let [text (cat/presets->edn c)
            [c2 n] (cat/import-presets cat/empty-catalog text)]
        (is (= 1 n)) (is (= (cat/user-presets c) (cat/user-presets c2)))
        (let [[c3 n3] (cat/import-presets c2 text)] (is (= 1 n3)) (is (= 1 (count (cat/user-presets c3))) "re-importing replaces, not duplicates"))))
    (testing "malformed input imports nothing"
      (doseq [bad ["" "not edn (" "{:presets []}" "{:groovehaus-presets 2 :presets []}" "{:groovehaus-presets 1 :presets [{:name 3}]}"]]
        (is (= [cat/empty-catalog 0] (cat/import-presets cat/empty-catalog bad)) bad)))
    (testing "built-in presets keep geometry, spots and masks too"
      (let [k (-> cat/empty-catalog (cat/set-adj p :crop [0.2 0.2 0.5 0.5]) (cat/set-adj p :local [{:id 1}]) (cat/apply-preset p "SILVER"))]
        (is (= [0.2 0.2 0.5 0.5] (:crop (cat/adj k p)))) (is (= [{:id 1}] (:local (cat/adj k p))))))))

(deftest setting-groups-cover-every-setting
  (let [grouped (set (concat (mapcat val cat/setting-groups)))]
    (is (= (set (keys pipeline/default-settings)) grouped) "no setting is missing from (or invented by) the groups")
    (is (= (count grouped) (count (mapcat val cat/setting-groups))) "and none is in two groups")
    (is (= (set cat/preserved-keys) (set (concat (:geometry cat/setting-groups) (:retouch cat/setting-groups)))))))

(deftest copy-and-paste-settings
  (let [p "/a.jpg" q "/b.jpg"
        c (-> cat/empty-catalog (cat/set-adj p :exposure 0.4) (cat/set-adj p :clarity 0.3) (cat/set-adj p :angle 3.0)
              (cat/set-adj p :curve [0 0.1 0.5 0.9 1]) (cat/set-adj q :angle -2.0) (cat/set-adj q :exposure -0.5))
        copied (cat/copy-settings c p)]
    (is (= 0.4 (:exposure copied))) (is (= 0.3 (:clarity copied))) (is (= [0 0.1 0.5 0.9 1] (:curve copied)))
    (is (not-any? #(contains? copied %) cat/preserved-keys) "geometry and retouching are not copied by default")
    (let [pasted (cat/paste-settings c q copied) a (cat/adj pasted q)]
      (is (= 0.4 (:exposure a))) (is (= -2.0 (:angle a)) "the target keeps its own geometry")
      (is (= "PASTE SETTINGS" (:label (peek (:history (cat/frame pasted q))))))
      (is (= 0.4 (:exposure (cat/adj (cat/undo (cat/undo pasted q) q) p)) ) "the source is untouched"))
    (testing "chosen groups only"
      (let [only (cat/copy-settings c p [:tone])]
        (is (= #{:exposure :contrast :highlights :shadows :whites :blacks :temp :tint} (set (keys only))))
        (is (= 0.4 (:exposure only)))))
    (testing "geometry can be copied on request"
      (is (= 3.0 (:angle (cat/copy-settings c p [:geometry])))))))

;; --- virtual copies and removal -------------------------------------------------------------

(deftest virtual-copies
  (let [[c id] (cat/add-shoot cat/empty-catalog "s" ["/a.jpg" "/b.jpg"])
        c (-> c (cat/set-adj "/a.jpg" :exposure 0.5) (cat/set-rating "/a.jpg" 4) (cat/add-keywords "/a.jpg" "x"))
        [c vc1] (cat/add-virtual-copy c id "/a.jpg")
        [c vc2] (cat/add-virtual-copy c id "/a.jpg")
        [c vc3] (cat/add-virtual-copy c id vc1)]
    (is (= ["/a.jpg#vc1" "/a.jpg#vc2" "/a.jpg#vc3"] [vc1 vc2 vc3]) "numbers are unique, and a copy of a copy is a copy of the file")
    (is (= ["/a.jpg" "/a.jpg#vc1" "/a.jpg#vc2" "/a.jpg#vc3" "/b.jpg"] (:paths (cat/shoot c id))) "next to the original")
    (is (= 0.5 (:exposure (cat/adj c vc1)))) (is (= 4 (cat/rating c vc1))) (is (= ["x"] (cat/keywords c vc1)))
    (testing "edits are independent"
      (let [c2 (cat/set-adj c vc1 :exposure -1.0)]
        (is (= 0.5 (:exposure (cat/adj c2 "/a.jpg"))))))
    (testing "names"
      (is (= "A VC1" (cat/frame-name vc1)))
      (is (= "A_VERY_LON VC12" (cat/frame-name "/x/a_very_long_name.jpg#vc12"))))
    (testing "removal forgets the copy's edits but never the original's"
      (let [r (cat/remove-frame c id vc2)]
        (is (not (some #{vc2} (:paths (cat/shoot r id))))) (is (not (contains? (:frames r) vc2)))
        (is (contains? (:frames r) vc1))))
    (testing "a frame still listed by another shoot keeps its edits"
      (let [[c2 id2] (cat/add-shoot c "t" ["/b.jpg"])
            c3 (-> c2 (cat/set-adj "/b.jpg" :exposure 0.2) (cat/remove-frame id "/b.jpg"))]
        (is (= 0.2 (:exposure (cat/adj c3 "/b.jpg"))))))))

;; --- search and sort ----------------------------------------------------------------------------

(deftest queries
  (let [ps ["/x/IMG_2.jpg" "/x/IMG_10.jpg" "/x/dj_set.jpg" "/x/crowd.jpg"]
        c (-> cat/empty-catalog
              (cat/set-rating "/x/IMG_2.jpg" 5) (cat/set-rating "/x/dj_set.jpg" 3)
              (cat/toggle-reject "/x/crowd.jpg") (cat/set-colour "/x/IMG_10.jpg" :red)
              (cat/add-keywords "/x/dj_set.jpg" "night, club") (cat/set-meta "/x/IMG_2.jpg" :title "Sunrise")
              (cat/set-adj "/x/IMG_10.jpg" :exposure 0.3))
        meta {"/x/IMG_2.jpg" {:make "Sony" :model "A7" :datetime-original "2024:05:02 10:00:00"}
              "/x/IMG_10.jpg" {:make "Canon" :datetime-original "2024:05:01 09:00:00"}
              "/x/dj_set.jpg" {:datetime-original "2024:05:03 22:00:00"}}
        q (fn [o] (cat/query c ps o #(meta %)))]
    (is (= ["/x/IMG_2.jpg" "/x/IMG_10.jpg" "/x/dj_set.jpg"] (q {})) "rejected frames are hidden by default, order kept")
    (is (= ps (q {:rejected :show})))
    (is (= ["/x/crowd.jpg"] (q {:rejected :only})))
    (testing "text search over names, titles, keywords and camera"
      (is (= ["/x/dj_set.jpg"] (q {:text "club"})))
      (is (= ["/x/IMG_2.jpg"] (q {:text "sunrise"})))
      (is (= ["/x/IMG_2.jpg"] (q {:text "SONY a7"})) "every word, any case")
      (is (= ["/x/IMG_10.jpg"] (q {:text "canon"})))
      (is (= [] (q {:text "nothing like this"}))))
    (testing "filters"
      (is (= ["/x/IMG_2.jpg" "/x/dj_set.jpg"] (q {:min-rating 3})))
      (is (= ["/x/IMG_10.jpg"] (q {:colour :red})))
      (is (= ["/x/dj_set.jpg"] (q {:keyword " Night "})))
      (is (= ["/x/IMG_10.jpg"] (q {:edited true}))))
    (testing "sorting"
      (is (= ["/x/dj_set.jpg" "/x/IMG_2.jpg" "/x/IMG_10.jpg"] (q {:sort :name})) "natural, case-insensitive order: dj, IMG_2, IMG_10")
      (is (= ["/x/IMG_2.jpg" "/x/dj_set.jpg" "/x/IMG_10.jpg"] (q {:sort :rating :dir :desc})))
      (is (= ["/x/IMG_10.jpg" "/x/IMG_2.jpg" "/x/dj_set.jpg"] (q {:sort :capture})))
      (is (= ["/x/dj_set.jpg" "/x/IMG_2.jpg" "/x/IMG_10.jpg"] (q {:sort :capture :dir :desc})))
      (is (= ["/x/IMG_10.jpg" "/x/IMG_2.jpg" "/x/dj_set.jpg"] (q {:sort :edited :dir :desc}))))))

(deftest catalog-location-can-be-moved-into-a-sync-folder
  (let [old (System/getProperty "groovehaus.catalog")]
    (try
      (System/setProperty "groovehaus.catalog" "/tmp/shared/cat.edn")
      (is (= "/tmp/shared/cat.edn" (.getPath (cat/default-file))))
      (finally (if old (System/setProperty "groovehaus.catalog" old) (System/clearProperty "groovehaus.catalog"))))
    (is (= "catalog.edn" (.getName (cat/default-file))))))

(deftest external-changes-are-detected-and_kept
  (let [d (.toFile (java.nio.file.Files/createTempDirectory "cat-sync" (into-array java.nio.file.attribute.FileAttribute [])))
        f (java.io.File. d "catalog.edn")]
    (is (not (cat/changed-on-disk? f 0)) "a missing file is not a change")
    (spit f "{}")
    (let [seen (.lastModified f)]
      (is (not (cat/changed-on-disk? f seen)))
      (.setLastModified f (+ seen 5000))
      (is (cat/changed-on-disk? f seen))
      (let [aside (cat/set-aside! f)]
        (is (= "{}" (slurp aside)))
        (is (re-find #"catalog\.edn\.conflict-\d+$" (.getName aside)))))))
