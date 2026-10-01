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


;; --- local adjustments and spots ---------------------------------------------------------

(deftest layers-add-select-update-delete
  (let [[a] (shoot-with "a.png")]
    (st/select! a)
    (is (= [] (st/layers)))
    (st/add-layer! :linear)
    (st/add-layer! :brush)
    (is (= [:linear :brush] (map :type (st/layers))))
    (is (= [1 2] (map :id (st/layers))))
    (is (= 2 (:local-sel @st/state)) "a new layer is selected")
    (is (= :brush (:type (st/selected-layer))))
    (testing "live edits do not add history; commit does"
      (let [h0 (count (:history (cat/frame (:catalog @st/state) a)))]
        (st/update-layer! 2 #(assoc-in % [:adj :exposure] 0.5))
        (is (= 0.5 (get-in (st/selected-layer) [:adj :exposure])))
        (is (= h0 (count (:history (cat/frame (:catalog @st/state) a)))))
        (st/commit! "T")
        (is (= (inc h0) (count (:history (cat/frame (:catalog @st/state) a)))))))
    (testing "other layers are untouched by an edit"
      (is (= {} (:adj (first (st/layers))))))
    (st/select-layer! 1)
    (st/delete-layer! 1)
    (is (= [2] (map :id (st/layers))))
    (is (nil? (:local-sel @st/state)) "deleting the selected layer clears the selection")
    (is (nil? (st/selected-layer)))
    (testing "ids stay unique after a delete"
      (st/add-layer! :radial)
      (is (= [2 3] (map :id (st/layers)))))
    (testing "the frame counts as edited, and the layers are part of its saved adjustments"
      (is (cat/edited? (:catalog @st/state) a))
      (is (= 2 (count (:local (cat/adj (:catalog @st/state) a))))))))

(deftest spots-add-and-remove
  (let [[a] (shoot-with "a.png")]
    (st/select! a)
    (is (= [] (st/spots)))
    (st/add-spot! {:x 0.2 :y 0.3 :r 0.02 :mode :heal})
    (st/add-spot! {:x 0.6 :y 0.4 :r 0.03 :mode :clone})
    (is (= [:heal :clone] (map :mode (st/spots))))
    (st/delete-spot! 0)
    (is (= [:clone] (map :mode (st/spots))))
    (st/clear-spots!)
    (is (= [] (st/spots)))
    (is (>= (count (:history (cat/frame (:catalog @st/state) a))) 5) "each action is an undoable history step")))

(deftest tool-settings
  (st/set-tool! :brush-size 0.07)
  (is (= 0.07 (get-in @st/state [:tool :brush-size])))
  (st/set-tool! :spot-mode :clone)
  (is (= :clone (get-in @st/state [:tool :spot-mode])))
  (is (= 0.5 (get-in @st/state [:tool :brush-feather])) "others keep their values"))

(deftest set-adjs-is-one-change
  (let [[a] (shoot-with "a.png") n (atom 0)]
    (st/select! a)
    (add-watch st/state ::count (fn [& _] (swap! n inc)))
    (st/set-adjs! {:exposure 0.4 :contrast 0.2 :tint -0.1})
    (remove-watch st/state ::count)
    (is (= 1 @n))
    (is (= [0.4 0.2 -0.1] ((juxt :exposure :contrast :tint) (st/cur-adj @st/state))))))

(deftest selection-and-bulk-actions
  (let [[a b c] (shoot-with "a.png" "b.png" "c.png")]
    (is (= [a] (st/selection)) "no multi-selection: the current frame")
    (st/select! b)
    (st/toggle-select! c)
    (is (= #{b c} (:sel @st/state)))
    (st/rate! 3)
    (is (= [0 3 3] (mapv #(cat/rating (:catalog @st/state) %) [a b c])) "rating applies to the whole selection")
    (st/rate! 3)
    (is (= [0 0 0] (mapv #(cat/rating (:catalog @st/state) %) [a b c])) "same rating again clears all")
    (st/reject!)
    (is (every? #(cat/rejected? (:catalog @st/state) %) [b c]))
    (is (not (cat/rejected? (:catalog @st/state) a)))
    (st/label! :red)
    (is (= [nil :red :red] (mapv #(cat/colour (:catalog @st/state) %) [a b c])))
    (st/add-keywords! "alpha, beta")
    (is (= ["alpha" "beta"] (:keywords (cat/frame (:catalog @st/state) c))))
    (st/remove-keyword! "alpha")
    (is (= ["beta"] (:keywords (cat/frame (:catalog @st/state) b))))
    (swap! st/state assoc :query {:rejected :show})
    (st/select! b)
    (st/range-select! c)
    (is (= #{b c} (:sel @st/state)) "range from the current frame (b) to c")
    (st/clear-selection!)
    (is (empty? (:sel @st/state)))))

(deftest library-query-uses-filters
  (let [[a b c] (shoot-with "a.png" "b.png" "c.png")]
    (st/select! b) (st/rate! 5)
    (is (= [a b c] (st/visible-frames @st/state)))
    (swap! st/state assoc :lf :picks)
    (is (= [b] (st/visible-frames @st/state)))
    (swap! st/state assoc :lf :all)
    (st/select! c) (st/reject!)
    (is (= [a b] (st/visible-frames @st/state)) "rejected frames are hidden by default")
    (swap! st/state assoc :lf :rejected)
    (is (= [c] (st/visible-frames @st/state)))))

(deftest copy-paste-virtual-copy-remove
  (let [[a b] (shoot-with "a.png" "b.png")]
    (st/select! a)
    (st/set-adj! :exposure 0.7) (st/commit! "EXPOSURE")
    (is (st/copy-settings!))
    (st/select! b)
    (is (= 1 (st/paste-settings!)))
    (is (== 0.7 (:exposure (cat/adj (:catalog @st/state) b))))
    (st/select! a)
    (is (= 1 (st/virtual-copy!)))
    (is (= 3 (count (st/frames))))
    (is (= (str a "#vc1") (:cur @st/state)))
    (is (= 1 (st/remove-frames!)))
    (is (= [a b] (st/frames)))))

(deftest presets-snapshots-undo
  (shoot-with "a.png")
  (st/set-adj! :exposure 0.5) (st/commit! "EXPOSURE")
  (is (st/save-preset! "WARM"))
  (is (not (st/save-preset! "  ")))
  (st/set-adj! :exposure -1.0) (st/commit! "EXPOSURE")
  (st/apply-user-preset! "WARM")
  (is (== 0.5 (:exposure (st/cur-adj @st/state))))
  (st/undo!)
  (is (== -1.0 (:exposure (st/cur-adj @st/state))))
  (st/redo!)
  (is (== 0.5 (:exposure (st/cur-adj @st/state))))
  (is (st/add-snapshot! "KEEP"))
  (st/set-adj! :exposure 1.5) (st/commit! "EXPOSURE")
  (st/apply-snapshot! 0)
  (is (== 0.5 (:exposure (st/cur-adj @st/state))))
  (let [f (File/createTempFile "presets" ".edn")]
    (is (= 1 (st/export-presets! f)))
    (st/delete-preset! "WARM")
    (is (= 1 (st/import-presets! f)))
    (.delete f)))

(deftest xmp-round-trip-through-import
  (let [d (tmp-dir) p (png! d "x.png")]
    (st/create-shoot! "T" [p])
    (st/rate! 4) (st/label! :blue) (st/add-keywords! "sea")
    (st/set-adj! :exposure 0.9) (st/commit! "EXPOSURE")
    (is (= 1 (st/write-xmp!)))
    (st/remove-frames!)
    (is (= 1 (st/import-paths! [p])))
    (let [c (:catalog @st/state)]
      (is (= 4 (cat/rating c p)))
      (is (= :blue (cat/colour c p)))
      (is (= ["sea"] (:keywords (cat/frame c p))))
      (is (== 0.9 (:exposure (cat/adj c p)))))))

(deftest survey-shows-only-the-selection
  (let [[a b c] (shoot-with "a.png" "b.png" "c.png")]
    (swap! st/state assoc :survey true)
    (is (not (st/survey? @st/state)) "one frame is not a survey")
    (is (= [a b c] (st/grid-frames @st/state)))
    (st/select! a) (st/toggle-select! c)
    (is (st/survey? @st/state))
    (is (= [a c] (st/grid-frames @st/state)))
    (st/nav! 1)
    (is (= [a b c] (st/grid-frames @st/state)) "moving drops the selection and so the survey")))

(deftest saving-over-an-external-change-keeps-the-other-copy
  (shoot-with "a.png")
  (st/save-now!)
  (let [f @st/catalog-file
        kept (fn [] (filter #(re-find #"conflict-" (.getName %)) (.listFiles (.getParentFile f))))]
    (is (.exists f))
    (spit f "{:shoots [] :frames {} :other-machine true}")
    (.setLastModified f (+ (.lastModified f) 10000))
    (st/rate! 3)
    (st/save-now!)
    (is (some #(re-find #"other-machine" (slurp %)) (kept)) "the other version was kept")
    (let [n (count (kept))]
      (st/rate! 4)
      (st/save-now!)
      (is (= n (count (kept))) "our own later saves are not conflicts"))))

(deftest hot-folder-imports-and-keeps-the-view
  (let [d (tmp-dir) lib (tmp-dir)]
    (st/create-shoot! "LIVE" [])
    (swap! st/state assoc :view :develop)
    (is (st/watch-folder! d))
    (is (= (.getPath d) (:watch @st/state)))
    (let [p (png! d "shot1.png")]
      (let [end (+ (System/currentTimeMillis) 8000)]
        (loop [] (when (and (empty? (st/frames)) (< (System/currentTimeMillis) end)) (Thread/sleep 100) (recur))))
      (is (= [p] (st/frames)))
      (is (= :develop (:view @st/state)) "a hot-folder import does not change the view"))
    (st/stop-watching!)
    (is (nil? (:watch @st/state)))))
