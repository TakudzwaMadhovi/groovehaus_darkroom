(ns groovehaus.vector.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [groovehaus.vector.core :as vc])
  (:import [javafx.scene Group Node]
           [javafx.scene.paint Color]
           [javafx.scene.shape ArcTo ClosePath Circle CubicCurveTo Ellipse Line LineTo MoveTo Path
            Polygon Polyline QuadCurveTo Rectangle]))

(defn- near? [a b] (< (Math/abs (- (double a) (double b))) 1e-6))
(defn- elements [^Path p] (vec (.getElements p)))

(deftest path-to-javafx-elements
  (let [els (elements (vc/path-d->fx "M10 20 L30 20 H50 V40 Z"))]
    (is (= [MoveTo LineTo LineTo LineTo ClosePath] (mapv class els)))
    (is (near? 10 (.getX ^MoveTo (els 0)))) (is (near? 20 (.getY ^MoveTo (els 0))))
    (is (near? 50 (.getX ^LineTo (els 2)))) (is (near? 20 (.getY ^LineTo (els 2))))
    (is (near? 50 (.getX ^LineTo (els 3)))) (is (near? 40 (.getY ^LineTo (els 3))))))

(deftest curves-translate-to-javafx
  (let [els (elements (vc/path-d->fx "M0 0 C1 2 3 4 5 6 s2 2 4 0 Q10 10 20 0 t10 0 a5 3 30 1 0 10 0"))
        [_ c s q t a] els]
    (is (= [MoveTo CubicCurveTo CubicCurveTo QuadCurveTo QuadCurveTo ArcTo] (mapv class els)))
    (let [^CubicCurveTo c c]
      (is (= [1.0 2.0 3.0 4.0 5.0 6.0]
             [(.getControlX1 c) (.getControlY1 c) (.getControlX2 c) (.getControlY2 c) (.getX c) (.getY c)])))
    (let [^CubicCurveTo s s] ; reflected control of (3,4) about (5,6) is (7,8); relative to (5,6)
      (is (= [7.0 8.0 7.0 8.0 9.0 6.0]
             [(.getControlX1 s) (.getControlY1 s) (.getControlX2 s) (.getControlY2 s) (.getX s) (.getY s)])))
    (let [^QuadCurveTo q q] (is (= [10.0 10.0 20.0 0.0] [(.getControlX q) (.getControlY q) (.getX q) (.getY q)])))
    (let [^QuadCurveTo t t] (is (= [30.0 -10.0 30.0 0.0] [(.getControlX t) (.getControlY t) (.getX t) (.getY t)])))
    (let [^ArcTo a a]
      (is (= [5.0 3.0 30.0 40.0 0.0] [(.getRadiusX a) (.getRadiusY a) (.getXAxisRotation a) (.getX a) (.getY a)]))
      (is (and (.isLargeArcFlag a) (not (.isSweepFlag a)))))))

(deftest malformed-path-is-an-error
  (is (thrown? clojure.lang.ExceptionInfo (vc/path-d->fx "M0 0 L"))))

(deftest primitive-shapes
  (let [r ^Rectangle (vc/hiccup->node [:rect {:x 1 :y 2 :width 30 :height 20 :rx 4}])]
    (is (= [1.0 2.0 30.0 20.0] [(.getX r) (.getY r) (.getWidth r) (.getHeight r)]))
    (is (= [8.0 8.0] [(.getArcWidth r) (.getArcHeight r)]) "rx is a radius, JavaFX arc is a diameter and ry defaults to rx"))
  (let [c ^Circle (vc/hiccup->node [:circle {:cx 5 :cy 6 :r 7}])]
    (is (= [5.0 6.0 7.0] [(.getCenterX c) (.getCenterY c) (.getRadius c)])))
  (let [e ^Ellipse (vc/hiccup->node [:ellipse {:cx 1 :cy 2 :rx 3 :ry 4}])]
    (is (= [1.0 2.0 3.0 4.0] [(.getCenterX e) (.getCenterY e) (.getRadiusX e) (.getRadiusY e)])))
  (let [l ^Line (vc/hiccup->node [:line {:x1 1 :y1 2 :x2 3 :y2 4 :stroke "red"}])]
    (is (= [1.0 2.0 3.0 4.0] [(.getStartX l) (.getStartY l) (.getEndX l) (.getEndY l)]))
    (is (= Color/RED (.getStroke l))))
  (is (= [0.0 0.0 10.0 5.0 20.0 0.0] (vec (.getPoints ^Polyline (vc/hiccup->node [:polyline {:points "0,0 10,5 20,0"}])))))
  (is (= 6 (count (.getPoints ^Polygon (vc/hiccup->node [:polygon {:points "0 0 10 0 5 9"}])))))
  (is (thrown? clojure.lang.ExceptionInfo (vc/hiccup->node [:polygon {:points "0 0 10"}]))))

(deftest svg-defaults-and-inheritance
  (let [g ^Group (vc/hiccup->node [:g {:fill "#00ff00" :stroke-width 3 :opacity 0.5}
                                   [:circle {:r 1}]
                                   [:circle {:r 1 :fill "#0000ff" :stroke "#ff0000"}]])
        [a b] (vec (.getChildren g))]
    (is (= Color/LIME (.getFill ^Circle a)) "fill inherited from <g>")
    (is (nil? (.getStroke ^Circle a)) "stroke defaults to none as in SVG")
    (is (= Color/BLUE (.getFill ^Circle b)) "own attribute overrides inherited")
    (is (= Color/RED (.getStroke ^Circle b)))
    (is (= 3.0 (.getStrokeWidth ^Circle a)))
    (is (= 0.5 (.getOpacity g)) "opacity applies to the group")
    (is (= 1.0 (.getOpacity ^Node a)) "opacity is not inherited per-child"))
  (is (= Color/BLACK (.getFill ^Circle (vc/hiccup->node [:circle {:r 1}]))) "SVG default fill is black"))

(deftest style-attribute-and-string-values
  (let [p ^Path (vc/hiccup->node [:path {:d "M0 0 L1 1" :style "fill: #ff0000; stroke:#0000ff;stroke-width:4" :stroke-dasharray "5 2"}])]
    (is (= Color/RED (.getFill p))) (is (= Color/BLUE (.getStroke p)))
    (is (= 4.0 (.getStrokeWidth p)))
    (is (= [5.0 2.0] (vec (.getStrokeDashArray p))))))

(deftest transforms-apply-in-svg-order
  (let [n ^Node (vc/hiccup->node [:g {:transform "translate(10 0) scale(2)"}])
        pt (.localToParent n 1.0 0.0)]
    (is (near? 12 (.getX pt)) "scale applies first, then translate")
    (is (near? 0 (.getY pt))))
  (let [pt (.localToParent ^Node (vc/hiccup->node [:g {:transform "rotate(90)"}]) 1.0 0.0)]
    (is (near? 0 (.getX pt))) (is (near? 1 (.getY pt))))
  (let [pt (.localToParent ^Node (vc/hiccup->node [:g {:transform "matrix(1 0 0 1 5 7)"}]) 0.0 0.0)]
    (is (near? 5 (.getX pt))) (is (near? 7 (.getY pt))))
  (let [pt (.localToParent ^Node (vc/hiccup->node [:g {:transform "matrix(2 0 0 3 0 0)"}]) 1.0 1.0)]
    (is (near? 2 (.getX pt))) (is (near? 3 (.getY pt))))
  (let [pt (.localToParent ^Node (vc/hiccup->node [:g {:transform "skewX(45)"}]) 0.0 1.0)]
    (is (near? 1 (.getX pt))))
  (is (thrown? clojure.lang.ExceptionInfo (vc/hiccup->node [:g {:transform "frobnicate(1)"}]))))

(deftest svg-viewbox-maps-to-width-height
  (let [n ^Node (vc/hiccup->node [:svg {:viewBox "0 0 100 50" :width 200 :height 100}])
        pt (.localToParent n 100.0 50.0)]
    (is (near? 200 (.getX pt))) (is (near? 100 (.getY pt))))
  (testing "meet + centre when aspect ratios differ"
    (let [pt (.localToParent ^Node (vc/hiccup->node [:svg {:viewBox "0 0 100 100" :width 200 :height 100}]) 0.0 0.0)]
      (is (near? 50 (.getX pt))))))

(deftest svg-string-and-hiccup-nesting
  (let [g ^Group (vc/svg->node "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"10\" height=\"10\">
                                  <title>x</title><defs/>
                                  <g id=\"grp\" fill=\"#ff0000\"><path id=\"p\" d=\"M0 0 L5 5 Z\"/><rect width=\"2\" height=\"2\"/></g>
                                </svg>")
        grp ^Group (first (.getChildren g))]
    (is (= "grp" (.getId grp)))
    (is (= 2 (count (.getChildren grp))))
    (is (= Color/RED (.getFill ^Path (first (.getChildren grp))))))
  (testing "seqs of children are spliced"
    (is (= 3 (count (.getChildren ^Group (vc/hiccup->node [:g (for [i (range 3)] [:circle {:r i}])])))))))

(deftest unsupported-and-hostile-input
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unsupported element" (vc/hiccup->node [:text {} "hi"])))
  (is (= 1 (count (.getChildren ^Group (vc/hiccup->node [:g [:text {}] [:circle {:r 1}]] {:strict? false})))))
  (is (thrown? Exception (vc/svg->hiccup "<!DOCTYPE svg [<!ENTITY x SYSTEM \"file:///etc/passwd\">]><svg>&x;</svg>")))
  (is (thrown? clojure.lang.ExceptionInfo (vc/hiccup->node "not hiccup"))))

(deftest effect-attribute
  (is (instance? javafx.scene.effect.DropShadow
                 (.getEffect (vc/hiccup->node [:circle {:r 1 :effect {:type :drop-shadow :radius 5}}])))))
