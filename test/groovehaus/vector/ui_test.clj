(ns groovehaus.vector.ui-test
  (:require [clojure.test :refer [deftest is testing]]
            [groovehaus.vector.trace :as t]
            [groovehaus.vector.ui :as ui])
  (:import [java.awt Color]
           [java.awt.image BufferedImage]
           [javafx.beans.property SimpleDoubleProperty]
           [javafx.scene Group Node]
           [javafx.scene.effect DropShadow]
           [javafx.scene.image ImageView]
           [javafx.scene.layout Pane StackPane]
           [javafx.scene.paint LinearGradient]
           [javafx.scene.shape Circle Path Rectangle]))

(defn- near? [a b] (< (Math/abs (- (double a) (double b))) 1e-9))
(defn- kids [o] (vec (.getChildren ^Group (:content o))))

(deftest overlay-is-transparent-clipped-and-passes-mouse-events
  (let [{:keys [^Pane pane]} (ui/make-overlay)]
    (is (.isMouseTransparent pane))
    (is (not (.isPickOnBounds pane)))
    (is (instance? Rectangle (.getClip pane)))
    (is (re-find #"transparent" (.getStyle pane)))))

(deftest shape-management
  (let [o (ui/make-overlay)]
    (ui/add-shape! o :a [:path {:d "M0 0 L10 10 Z"}])
    (ui/add-shape! o :b [:circle {:r 5}])
    (is (= [:a :b] (ui/shape-ids o)))
    (testing "replacing an id re-adds it on top and leaves no orphan"
      (let [old (ui/shape o :a)]
        (ui/add-shape! o :a [:rect {:width 1 :height 1}])
        (is (= [:b :a] (ui/shape-ids o)))
        (is (not (identical? old (ui/shape o :a))))
        (is (= 2 (count (kids o))))))
    (testing "accepts SVG strings and ready-made nodes"
      (ui/add-shape! o :svg "<svg xmlns=\"http://www.w3.org/2000/svg\"><circle r=\"3\"/></svg>")
      (let [c (Circle. 4.0)] (ui/add-shape! o :node c) (is (identical? c (ui/shape o :node)))))
    (is (thrown? clojure.lang.ExceptionInfo (ui/add-shape! o :bad 42)))
    (ui/remove-shape! o :svg)
    (is (nil? (ui/shape o :svg)))
    (ui/clear! o)
    (is (empty? (kids o))) (is (empty? (ui/shape-ids o)))))

(deftest full-styling-through-the-overlay
  (let [o (ui/make-overlay)
        ^Path n (ui/add-path! o :p "M0 0 L100 0 L50 80 Z"
                              {:style {:fill {:type :linear-gradient :from [0 0] :to [1 1]
                                              :stops [[0 "#ffff00"] [1 "#ff00ff"]]}
                                       :stroke "#ffffff" :stroke-width 3 :opacity 0.8
                                       :effect {:type :drop-shadow :radius 12 :offset-x 4 :offset-y 5}}})]
    (is (instance? LinearGradient (.getFill n)))
    (is (= 3.0 (.getStrokeWidth n)))
    (is (= 0.8 (.getOpacity n)))
    (let [e ^DropShadow (.getEffect n)] (is (= [12.0 4.0 5.0] [(.getRadius e) (.getOffsetX e) (.getOffsetY e)])))
    (ui/set-style! o :p {:opacity 0.3 :fill "#00ff00"})
    (is (= 0.3 (.getOpacity n)))
    (is (not (instance? LinearGradient (.getFill n))))
    (ui/set-visible! o :p false)
    (is (not (.isVisible n)))
    (is (thrown? clojure.lang.ExceptionInfo (ui/set-style! o :missing {:opacity 1})))
    (is (thrown? clojure.lang.ExceptionInfo (ui/set-visible! o :missing true)))))

(deftest hiccup-style-and-explicit-style-compose
  (let [o (ui/make-overlay)
        ^Circle c (ui/add-shape! o :c [:circle {:r 4 :fill "#ff0000" :stroke-width 2}] {:style {:fill "#0000ff"}})]
    (is (= javafx.scene.paint.Color/BLUE (.getFill c)) "explicit :style wins")
    (is (= 2.0 (.getStrokeWidth c)))))

(deftest content-scales-from-image-pixels-to-view
  (let [o (ui/make-overlay)
        vw (SimpleDoubleProperty. 800.0) vh (SimpleDoubleProperty. 400.0)
        iw (SimpleDoubleProperty. 4000.0) ih (SimpleDoubleProperty. 2000.0)
        scale (:scale o) ^Pane pane (:pane o)]
    (ui/bind-size! o vw vh iw ih)
    (is (near? 0.2 (.getX scale))) (is (near? 0.2 (.getY scale)))
    (is (= [800.0 400.0] [(.getPrefWidth pane) (.getPrefHeight pane)]))
    (is (= [800.0 400.0] [(.getMaxWidth pane) (.getMaxHeight pane)]))
    (testing "a shape in image pixels lands on the right screen pixel"
      (ui/add-shape! o :dot [:circle {:cx 2000 :cy 1000 :r 10}])
      (let [pt (.localToParent ^Node (:content o) 2000.0 1000.0)]
        (is (near? 400 (.getX pt))) (is (near? 200 (.getY pt)))))
    (testing "resize follows the view"
      (.set vw 400.0) (.set vh 200.0)
      (is (near? 0.1 (.getX scale))) (is (near? 0.1 (.getY scale)))
      (is (= 400.0 (.getPrefWidth pane))))
    (testing "new image dimensions rescale"
      (.set iw 400.0) (.set ih 200.0)
      (is (near? 1.0 (.getX scale))))
    (testing "unknown sizes fall back to identity instead of NaN/Infinity"
      (.set iw 0.0)
      (is (near? 1.0 (.getX scale))))))

(deftest fit-scale-guards
  (is (= 0.5 (ui/fit-scale 50.0 100.0)))
  (is (= 1.0 (ui/fit-scale 0.0 100.0)))
  (is (= 1.0 (ui/fit-scale 50.0 0.0))))

(deftest mount-on-image-view
  (let [iv (doto (ImageView.) (.setFitWidth 640.0) (.setFitHeight 480.0))
        sp (StackPane.)
        o (ui/make-overlay)]
    (.add (.getChildren sp) iv)
    (ui/mount! sp iv o)
    (is (= [iv (:pane o)] (vec (.getChildren sp))) "overlay sits above the ImageView")
    (ui/mount! sp iv o)
    (is (= 2 (count (.getChildren sp))) "mounting twice does not duplicate the pane")
    (is (= [640.0 480.0] [(.getPrefWidth ^Pane (:pane o)) (.getPrefHeight ^Pane (:pane o))]))))

(deftest traced-mask-renders-as-an-overlay-path
  (let [img (BufferedImage. 30 30 BufferedImage/TYPE_INT_RGB)
        g (.createGraphics img)]
    (.setColor g Color/WHITE) (.fillRect g 5 5 10 10) (.dispose g)
    (let [o (ui/make-overlay)
          ^Path p (ui/add-path! o :cutout (t/trace->path-d img) {:style {:fill "#ff8800" :fill-rule :even-odd}})]
      (is (= 5 (count (.getElements p))) "M + 3 L + Z")
      (is (= javafx.scene.shape.FillRule/EVEN_ODD (.getFillRule p))))))
