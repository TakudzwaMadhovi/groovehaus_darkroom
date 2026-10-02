(ns darkroom.ui.state-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [darkroom.catalog :as cat]
            [darkroom.dav-fixture]
            [darkroom.dcp-fixture]
            [darkroom.imaging.core]
            [darkroom.imaging.exif]
            [darkroom.imaging.export]
            [darkroom.imaging.lens]
            [darkroom.imaging.panorama-test]
            [darkroom.imaging.scene]
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

(deftest lens-profile-from-the-database
  (let [d (tmp-dir)
        img (darkroom.imaging.core/image 8 8 (int-array 64 (unchecked-int 0xFF808080)))
        tags {:make "NIKON" :lens-model "Nikkor 50mm f/1.8" :focal-length [50 1] :focal-length-35mm 50}
        f (darkroom.imaging.export/save! img {:dir (.getPath d) :name "lensy" :format :jpeg
                                              :exif (darkroom.imaging.exif/exif-block tags {})})
        db (darkroom.imaging.lens/parse-database
             (.getBytes (str "<lensdatabase><lens><maker>Nikon</maker><model>Nikkor 50mm f/1.8</model><cropfactor>1</cropfactor>"
                             "<calibration><distortion model=\"poly3\" focal=\"50\" k1=\"-0.02\"/></calibration></lens></lensdatabase>") "UTF-8"))]
    (st/create-shoot! "L" [(.getPath f)])
    (reset! st/lens-db [])
    (is (nil? (st/apply-lens-profile!)) "no database yet")
    (is (nil? (:lens-profile (st/cur-adj @st/state))))
    (reset! st/lens-db db)
    (let [p (st/apply-lens-profile!)]
      (is (= "Nikon Nikkor 50mm f/1.8" (:name p)))
      (is (= p (:lens-profile (st/cur-adj @st/state))))
      (is (= "LENS PROFILE" (:label (last (:history (cat/frame (:catalog @st/state) (.getPath f))))))))
    (st/clear-lens-profile!)
    (is (nil? (:lens-profile (st/cur-adj @st/state))))
    (testing "a lens that is not in the database"
      (reset! st/lens-db (darkroom.imaging.lens/parse-database (.getBytes "<lensdatabase/>" "UTF-8")))
      (is (nil? (st/apply-lens-profile!))))
    (reset! st/lens-db [])))

(deftest camera-profiles-are-listed-applied-and-matched
  (let [d (tmp-dir)
        cm darkroom.dcp-fixture/srgb-from-xyz-d65
        mk (fn [n cam] (let [f (java.io.File. d n)]
                         (java.nio.file.Files/write (.toPath f)
                           ^bytes (darkroom.dcp-fixture/dcp-bytes [[50936 2 n] [50708 2 cam] [50721 10 cm] [50778 3 [21]]])
                           (into-array java.nio.file.OpenOption []))
                         f))
        a (mk "Neutral" "Test Camera X") b (mk "Other" "Some Other Camera")
        junk (let [f (java.io.File. d "junk.dcp")] (spit f "not a profile") f)
        jpg (darkroom.imaging.export/save! (darkroom.imaging.core/image 8 8 (int-array 64 (unchecked-int 0xFF808080)))
                                           {:dir (.getPath d) :name "shot" :format :jpeg
                                            :exif (darkroom.imaging.exif/exif-block {:make "TEST" :model "Test Camera X"} {})})]
    (st/create-shoot! "P" [(.getPath jpg)])
    (is (= {:added 2 :failed ["junk.dcp"]} (st/add-camera-profiles! [a b junk])))
    (is (= ["Neutral" "Other"] (mapv :name (st/camera-profiles))))
    (is (= {:added 2 :failed []} (st/add-camera-profiles! [a b])) "adding again does not duplicate")
    (is (= 2 (count (:camera-profiles (:catalog @st/state)))))
    (let [hit (st/auto-camera-profile!)]
      (is (= "Neutral" (:name hit)) "matched on the EXIF camera model")
      (is (= (.getPath a) (:camera-profile (st/cur-adj @st/state))))
      (is (= "CAMERA PROFILE" (:label (last (:history (cat/frame (:catalog @st/state) (.getPath jpg))))))))
    (st/toggle-profile-curve!)
    (is (true? (:camera-profile-curve (st/cur-adj @st/state))))
    (testing "a preset does not change the profile"
      (st/apply-preset! "SILVER")
      (is (= (.getPath a) (:camera-profile (st/cur-adj @st/state)))))
    (st/remove-camera-profile! (.getPath a))
    (is (nil? (:camera-profile (st/cur-adj @st/state))) "frames using a removed profile go back to the default")
    (is (= ["Other"] (mapv :name (st/camera-profiles))))
    (st/remove-camera-profile! (.getPath b))
    (is (nil? (st/auto-camera-profile!)) "nothing to match")))

(deftest ai-subject-layer-needs-a-working-model
  (shoot-with "a.png")
  (st/add-layer! :subject-ai)
  (is (empty? (st/layers)) "no model chosen: nothing added")
  (let [bad (java.io.File/createTempFile "bad" ".onnx")]
    (spit bad "nope")
    (is (false? (st/set-segment-model! bad)))
    (is (nil? (st/segment-model)) "a model that cannot load is not remembered"))
  (let [model (System/getenv "GROOVEHAUS_TEST_ONNX")]
    (if-not (and model (.isFile (java.io.File. ^String model)))
      (println "  (skipped: GROOVEHAUS_TEST_ONNX not set)")
      (do (is (true? (st/set-segment-model! (java.io.File. ^String model))))
          (st/add-layer! :subject-ai)
          (is (= [:subject-ai] (mapv :type (st/layers))))
          (is (= model (get-in (first (st/layers)) [:shape :model])))))))

(defn- await-frames [n ms]
  (let [end (+ (System/currentTimeMillis) ms)]
    (loop [] (cond (>= (count (st/frames)) n) true (> (System/currentTimeMillis) end) false :else (do (Thread/sleep 100) (recur))))))

(deftest hdr-merge-and-panorama-import-their-result
  (let [d (tmp-dir)
        wd (#'darkroom.imaging.panorama-test/world 240 160 3)
        save (fn [name sc tags] (.getPath (darkroom.imaging.export/save!
                                            (darkroom.imaging.scene/->argb sc :srgb)
                                            {:dir (.getPath d) :name name :format :jpeg :quality 0.98
                                             :exif (when tags (darkroom.imaging.exif/exif-block tags {}))})))
        scale (fn [{:keys [width height data]} k] (let [out (float-array (alength ^floats data))]
                                                    (dotimes [i (alength ^floats data)] (aset out i (float (min 1.0 (* k (aget ^floats data i))))))
                                                    (darkroom.imaging.scene/image width height out)))
        a (save "b1" (scale wd 0.5) {:exposure-time [1 400] :f-number [8 1] :iso 100})
        b (save "b2" (scale wd 1.0) {:exposure-time [1 200] :f-number [8 1] :iso 100})
        c (save "b3" (scale wd 2.0) {:exposure-time [1 100] :f-number [8 1] :iso 100})]
    (st/create-shoot! "BRACKET" [a b c])
    (testing "one frame selected: nothing happens"
      (st/merge-hdr!)
      (Thread/sleep 300)
      (is (= 3 (count (st/frames)))))
    (st/select! a) (st/toggle-select! b) (st/toggle-select! c)
    (st/merge-hdr!)
    (is (await-frames 4 60000) "the merged frame joins the shoot")
    (let [new (last (st/frames))]
      (is (re-find #"b1-HDR\.tif$" new))
      (is (= new (:cur @st/state)) "and is selected")
      (is (.isFile (java.io.File. ^String new))))))

(deftest panorama-import-gets-a-crop
  (let [d (tmp-dir)
        wd (#'darkroom.imaging.panorama-test/world 580 240 11)
        save (fn [name x0] (.getPath (darkroom.imaging.export/save!
                                       (darkroom.imaging.scene/->argb (#'darkroom.imaging.panorama-test/window wd x0 0 360 240 1.0) :srgb)
                                       {:dir (.getPath d) :name name :format :jpeg :quality 0.98})))
        a (save "p1" 0) b (save "p2" 220)]
    (st/create-shoot! "PANO" [a b])
    (st/select! a) (st/toggle-select! b)
    (st/stitch-panorama!)
    (is (await-frames 3 90000))
    (let [new (last (st/frames))]
      (is (re-find #"p1-Pano\.tif$" new))
      (let [crop (:crop (cat/adj (:catalog @st/state) new))]
        (is (= 4 (count crop)))
        (is (every? #(<= 0.0 % 1.0) crop))
        (is (> (nth crop 2) 0.9) "the crop keeps nearly the whole canvas")))))

(deftest icc-input-profiles-are-listed-like-dcp-ones
  (let [d (tmp-dir) f (java.io.File. d "mycam.icc")]
    (java.nio.file.Files/write (.toPath f) ^bytes (darkroom.imaging.color/icc-bytes :adobe-rgb) (into-array java.nio.file.OpenOption []))
    (shoot-with "a.png")
    (is (= {:added 1 :failed []} (st/add-camera-profiles! [f])))
    (is (= ["Adobe RGB (1998)"] (mapv :name (st/camera-profiles))))
    (st/set-camera-profile! (.getPath f))
    (is (= (.getPath f) (:camera-profile (st/cur-adj @st/state))))
    (st/remove-camera-profile! (.getPath f))))

(defn- await-pred [pred ms]
  (let [end (+ (System/currentTimeMillis) ms)]
    (loop [] (cond (pred) true (> (System/currentTimeMillis) end) false :else (do (Thread/sleep 50) (recur))))))

(deftest catalog-sync-round-trip-between-two-computers
  (let [dav (darkroom.dav-fixture/start! {})
        a-dir (tmp-dir) b-dir (tmp-dir)
        pa (png! a-dir "one.png")]
    (try
      (reset! st/sync-password "p")
      ;; computer A
      (st/create-shoot! "TRIP" [pa])
      (st/rate! 4)
      (st/update-catalog! assoc :camera-profiles ["/only/on/a.dcp"])
      (st/set-sync! (:url dav) "u" (str (.getPath a-dir) "=/Volumes/Photos"))
      (is (= [[(.getPath a-dir) "/Volumes/Photos"]] (:path-map (st/sync-config))))
      (st/sync-push!)
      (is (await-pred #(:etag (st/sync-config)) 10000) "the upload finishes and records the server's version")
      (let [remote (cat/parse-text (:body @(:state dav)))]
        (is (= ["/Volumes/Photos/one.png"] (:paths (first (:shoots remote)))) "paths are written in the shared form")
        (is (= 4 (cat/rating remote "/Volumes/Photos/one.png")))
        (is (not-any? #(contains? remote %) cat/machine-keys) "nothing about computer A itself is shared"))
      ;; computer B: another path for the same photos, its own settings
      (reset! st/state initial)
      (reset! st/catalog-file (java.io.File. b-dir "catalog.edn"))
      (st/create-shoot! "OTHER" [])
      (st/update-catalog! assoc :camera-profiles ["/only/on/b.dcp"])
      (st/set-sync! (:url dav) "u" (str (.getPath b-dir) "=/Volumes/Photos"))
      (st/sync-pull!)
      (is (await-pred #(= ["TRIP"] (mapv :name (:shoots (:catalog @st/state)))) 10000) "the shoot arrived")
      (let [c (:catalog @st/state) bp (str (.getPath b-dir) "/one.png")]
        (is (= [bp] (:paths (first (:shoots c)))) "paths mapped to this computer")
        (is (= 4 (cat/rating c bp)))
        (is (= ["/only/on/b.dcp"] (:camera-profiles c)) "its own settings survived")
        (is (some? (:etag (:sync c))))
        (is (some #(re-find #"^catalog-before-pull-" (.getName %)) (.listFiles b-dir)) "the old catalog was kept"))
      ;; B changes something and pushes with the right base: fine; A still has the old base: conflict
      (st/update-catalog! (fn [c] (cat/assign-rating c (str (.getPath b-dir) "/one.png") 2)))
      (st/sync-push!)
      (is (await-pred #(= 2 (cat/rating (cat/parse-text (:body @(:state dav))) "/Volumes/Photos/one.png")) 10000))
      (reset! st/state initial)
      (st/create-shoot! "A AGAIN" [])
      (st/set-sync! (:url dav) "u" "")
      (st/update-catalog! assoc-in [:sync :etag] "\"v1\"")        ; stale base
      (st/sync-push!)
      (is (await-pred #(= "REMOTE CATALOG CHANGED — PULL FIRST" (:msg (:toast @st/state))) 10000))
      (finally ((:stop! dav)) (reset! st/sync-password "")))))

(deftest sync-needs-an-address-and-parses-path-maps
  (shoot-with "a.png")
  (st/sync-push!)
  (is (= "SET THE SYNC FOLDER ADDRESS FIRST" (:msg (:toast @st/state))))
  (is (= [["/a" "/b"] ["C:/x" "D:/y"]] (st/parse-path-map " /a = /b ; C:/x=D:/y ; ; broken ")))
  (is (= [] (st/parse-path-map ""))))

(deftest phone-companion-reads-and-rates-through-the-catalog
  (let [d (tmp-dir) p (png! d "pic.png")
        http (fn [method url & {:keys [body headers]}]
               (let [c (-> (java.net.http.HttpClient/newBuilder) (.followRedirects java.net.http.HttpClient$Redirect/NEVER) .build)
                     b (java.net.http.HttpRequest/newBuilder (java.net.URI/create url))]
                 (doseq [[k v] headers] (.header b k v))
                 (.method b method (if body (java.net.http.HttpRequest$BodyPublishers/ofString body) (java.net.http.HttpRequest$BodyPublishers/noBody)))
                 (let [r (.send c (.build b) (java.net.http.HttpResponse$BodyHandlers/ofByteArray))]
                   {:status (.statusCode r) :body (.body r)})))]
    (st/create-shoot! "PHONE" [p])
    (st/start-remote!)
    (try
      (is (st/remote-running?))
      (let [url (:remote-url @st/state)
            [_ port token] (re-find #":(\d+)/\?t=([0-9a-f]{32})$" url)
            base (str "http://127.0.0.1:" port)
            h {"Cookie" (str "gh=" token)}
            txt #(String. ^bytes (:body %) "UTF-8")]
        (is (= 403 (:status (http "GET" (str base "/api/shoots")))) "no token, no access")
        (let [shoots (txt (http "GET" (str base "/api/shoots") :headers h))]
          (is (re-find #"\"name\":\"PHONE\",\"count\":1" shoots)))
        (let [sid (:id (first (:shoots (:catalog @st/state))))
              fs (txt (http "GET" (str base "/api/shoot/" sid) :headers h))
              fid (second (re-find #"\"id\":\"([0-9a-f]{12})\"" fs))]
          (is (re-find #"\"name\":\"PIC\"" fs))
          (is (not (re-find #"darkroom-state" fs)) "no file paths in the listing")
          (let [thumb (http "GET" (str base "/api/thumb/" fid) :headers h)]
            (is (= 200 (:status thumb)))
            (is (= [-1 -40] (take 2 (:body thumb))) "a JPEG"))
          (let [prev (http "GET" (str base "/api/preview/" fid) :headers h)]
            (is (= 200 (:status prev))) (is (= [-1 -40] (take 2 (:body prev)))))
          (is (= 404 (:status (http "GET" (str base "/api/thumb/ffffffffffff") :headers h))) "only catalog frames can be reached")
          (let [r (http "POST" (str base "/api/frame/" fid) :headers (assoc h "X-Requested-With" "gh") :body "rating=4&colour=green&reject=1")]
            (is (= 200 (:status r)))
            (is (= 4 (cat/rating (:catalog @st/state) p)))
            (is (= :green (cat/colour (:catalog @st/state) p)))
            (is (cat/rejected? (:catalog @st/state) p)))
          (http "POST" (str base "/api/frame/" fid) :headers (assoc h "X-Requested-With" "gh") :body "colour=&reject=0&rating=0")
          (is (nil? (cat/colour (:catalog @st/state) p)))
          (is (not (cat/rejected? (:catalog @st/state) p)))
          (is (= 0 (cat/rating (:catalog @st/state) p)))))
      (finally (st/stop-remote!)))
    (is (not (st/remote-running?)))
    (is (nil? (:remote-url @st/state)))))
