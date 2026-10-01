(ns darkroom.ui.state-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [darkroom.catalog :as cat]
            [darkroom.ui.state :as st])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(def ^:private initial @st/state)

(defn- tmp-dir ^File [] (.toFile (Files/createTempDirectory "darkroom-state" (into-array FileAttribute []))))

(defn- png! [^File dir n]
  (let [f (File. dir ^String n)]
    (javax.imageio.ImageIO/write (java.awt.image.BufferedImage. 4 4 java.awt.image.BufferedImage/TYPE_INT_RGB) "png" f)
    (.getPath f)))

(use-fixtures :each
  (fn [t]
    (reset! st/state initial)
    (reset! st/catalog-file (File. (tmp-dir) "catalog.edn"))
    (t)))

(defn- shoot-with [& names]
  (let [d (tmp-dir) ps (mapv #(png! d %) names)]
    (st/create-shoot! "TEST" ps)
    ps))

(deftest create-and-select
  (let [[a b c] (shoot-with "a.png" "b.png" "c.png")]
    (is (= [a b c] (st/frames)))
    (is (= a (:cur @st/state)) "creating a shoot selects its first frame")
    (is (nil? (st/create-shoot! "  " [])) "blank names are rejected")
    (st/create-shoot! "second" [])
    (is (= "s2" (:shoot @st/state)))
    (is (= [] (st/frames)))
    (st/select-shoot! "s1")
    (is (= [a b c] (st/frames)))
    (is (= a (:cur @st/state)))))

(deftest navigation-wraps
  (let [[a b c] (shoot-with "a.png" "b.png" "c.png")]
    (st/nav! 1) (is (= b (:cur @st/state)))
    (st/nav! 1) (st/nav! 1) (is (= a (:cur @st/state)) "wraps forward")
    (st/nav! -1) (is (= c (:cur @st/state)) "wraps backward")))

(deftest go-develop
  (let [[a b] (shoot-with "a.png" "b.png")]
    (st/go! :develop b)
    (is (= [:develop b] [(:view @st/state) (:cur @st/state)]))
    (st/go! :library)
    (is (= :library (:view @st/state)))
    (testing "a frame outside the shoot jumps to the first frame"
      (swap! st/state assoc :cur "/elsewhere.png")
      (st/go! :develop)
      (is (= a (:cur @st/state))))
    (testing "an empty shoot stays in the library"
      (st/create-shoot! "EMPTY" [])
      (swap! st/state assoc :view :library :cur nil)
      (st/go! :develop)
      (is (= :library (:view @st/state))))))

(deftest ratings-edits-and-filters
  (let [[a b c] (shoot-with "a.png" "b.png" "c.png")]
    (st/toggle-pick! b)
    (swap! st/state assoc :cur c) (st/rate! 3)
    (swap! st/state assoc :cur a) (st/set-adj! :exposure 0.5) (st/commit! "EXPOSURE")
    (is (= [a b c] (st/visible-frames @st/state)))
    (swap! st/state assoc :lf :picks)   (is (= [b] (st/visible-frames @st/state)))
    (swap! st/state assoc :lf :edited)  (is (= [a] (st/visible-frames @st/state)))
    (is (= 0.5 (:exposure (st/cur-adj @st/state))))
    (st/revert! 0)
    (is (= 0.0 (:exposure (st/cur-adj @st/state))) "revert restores the imported state")
    (st/apply-preset! "SILVER")
    (is (= 1.0 (:bw (st/cur-adj @st/state))))))

(deftest import-adds-to-current-shoot
  (let [[a] (shoot-with "a.png")
        d   (tmp-dir) x (png! d "x.png") y (png! d "y.png")]
    (is (= 2 (st/import-paths! [x y])))
    (is (= x (:cur @st/state)) "selects the first new frame")
    (is (= [a x y] (st/frames)))
    (is (= 0 (st/import-paths! [x y])) "duplicates are not added")
    (testing "importing with no shoot creates FIRST SHOOT"
      (reset! st/state initial)
      (is (= 1 (st/import-paths! [x])))
      (is (= "FIRST SHOOT" (:name (cat/shoot (:catalog @st/state) (:shoot @st/state))))))
    (testing "folders are scanned, non-images ignored"
      (spit (File. d "notes.txt") "hi")
      (is (= #{x y} (set (st/image-paths [d (File. d "notes.txt")])))))))

(deftest catalog-persists
  (let [[a] (shoot-with "a.png")]
    (st/set-adj! :exposure 0.25) (st/commit! "EXPOSURE")
    (st/save-now!)
    (reset! st/state initial)
    (st/load-catalog!)
    (is (= a (:cur @st/state)))
    (is (= 0.25 (:exposure (st/cur-adj @st/state))))))

(deftest open-file-creates-shoot-from-folder
  (let [d (tmp-dir) a (png! d "a.png") b (png! d "b.png")]
    (st/open-file! (File. b))
    (is (= [:develop b] [(:view @st/state) (:cur @st/state)]))
    (is (= [a b] (st/frames)))
    (is (= (cat/shoot-name (.getName d)) (:name (cat/shoot (:catalog @st/state) (:shoot @st/state)))))))

(deftest keyboard-selection-moves-through-visible-frames
  (let [[a b c d] (shoot-with "a.png" "b.png" "c.png" "d.png")]
    (st/select! a)
    (st/move! 1)  (is (= b (:cur @st/state)))
    (st/move! 2)  (is (= d (:cur @st/state)) "a row down (2 columns) from b")
    (st/move! 5)  (is (= d (:cur @st/state)) "stops at the last frame (no wrap)")
    (st/move! -9) (is (= a (:cur @st/state)) "stops at the first frame")
    (testing "only visible (filtered) frames are visited"
      (st/toggle-pick! a) (st/toggle-pick! c)
      (swap! st/state assoc :lf :picks)
      (st/select! a)
      (st/move! 1)
      (is (= c (:cur @st/state)) "skips b, which the PICKS filter hides"))))
