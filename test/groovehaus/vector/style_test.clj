(ns groovehaus.vector.style-test
  (:require [clojure.test :refer [deftest is testing]]
            [groovehaus.vector.style :as s])
  (:import [javafx.scene.effect DropShadow]
           [javafx.scene.paint Color CycleMethod LinearGradient RadialGradient]
           [javafx.scene.shape FillRule Path Rectangle StrokeLineCap StrokeLineJoin]))

(defn- near? [a b] (< (Math/abs (- (double a) (double b))) 1e-3))

(deftest colours
  (is (= (Color/web "#ff8800") (s/->color "#ff8800")))
  (is (< (Math/abs (- 0.5 (.getOpacity (s/->color "#ff880080")))) 0.01) "8-digit hex is RRGGBBAA")
  (is (= Color/RED (s/->color Color/RED)))
  (let [c (s/->color [0.2 0.4 0.6 0.5])]
    (is (near? 0.2 (.getRed c))) (is (near? 0.5 (.getOpacity c))))
  (is (thrown? clojure.lang.ExceptionInfo (s/->color {:nope 1}))))

(deftest paints
  (is (nil? (s/->paint "none"))) (is (nil? (s/->paint nil)))
  (is (near? 0.25 (.getOpacity ^Color (s/->paint "#ff0000" 0.25))))
  (let [g ^LinearGradient (s/->paint {:type :linear-gradient :from [0 0] :to [1 1]
                                      :stops [[1 "#0000ff"] [0 "#ff0000"]]})]
    (is (instance? LinearGradient g))
    (is (.isProportional g))
    (is (= [0.0 1.0] (mapv #(.getOffset ^javafx.scene.paint.Stop %) (.getStops g))) "stops are sorted")
    (is (= Color/RED (.getColor ^javafx.scene.paint.Stop (first (.getStops g)))))
    (is (= CycleMethod/NO_CYCLE (.getCycleMethod g)))
    (is (= [1.0 1.0] [(.getEndX g) (.getEndY g)])))
  (let [g ^RadialGradient (s/->paint {:type :radial-gradient :center [0.3 0.4] :radius 0.7 :cycle :reflect
                                      :stops [[0 "white"] [1 "black"]]} 0.5)]
    (is (instance? RadialGradient g))
    (is (= [0.3 0.4 0.7] [(.getCenterX g) (.getCenterY g) (.getRadius g)]))
    (is (= CycleMethod/REFLECT (.getCycleMethod g)))
    (is (near? 0.5 (.getOpacity (.getColor ^javafx.scene.paint.Stop (first (.getStops g))))) "opacity folded into stops"))
  (is (thrown? clojure.lang.ExceptionInfo (s/->paint {:type :linear-gradient :stops [[0 "red"]]})))
  (is (thrown? clojure.lang.ExceptionInfo (s/->paint {:type :conic}))))

(deftest effects
  (let [e ^DropShadow (s/->effect {:type :drop-shadow :radius 12 :spread 0.2 :offset-x 3 :offset-y 4 :color "#112233"})]
    (is (= [12.0 0.2 3.0 4.0] [(.getRadius e) (.getSpread e) (.getOffsetX e) (.getOffsetY e)]))
    (is (= (Color/web "#112233") (.getColor e))))
  (is (nil? (s/->effect nil)))
  (is (thrown? clojure.lang.ExceptionInfo (s/->effect {:type :sparkle}))))

(deftest apply-style-to-shapes
  (let [p (Path.)]
    (s/apply-style! p {:fill "#ff0000" :stroke {:type :linear-gradient :stops [[0 "red"] [1 "blue"]]}
                       :stroke-width 4 :stroke-linecap :round :stroke-linejoin "bevel"
                       :stroke-dasharray [6 3] :fill-rule :even-odd :opacity 0.6
                       :effect {:type :drop-shadow}})
    (is (= Color/RED (.getFill p)))
    (is (instance? LinearGradient (.getStroke p)))
    (is (= 4.0 (.getStrokeWidth p)))
    (is (= StrokeLineCap/ROUND (.getStrokeLineCap p)))
    (is (= StrokeLineJoin/BEVEL (.getStrokeLineJoin p)))
    (is (= [6.0 3.0] (vec (.getStrokeDashArray p))))
    (is (= FillRule/EVEN_ODD (.getFillRule p)))
    (is (= 0.6 (.getOpacity p)))
    (is (instance? DropShadow (.getEffect p)))
    (testing "partial update leaves other properties alone"
      (s/apply-style! p {:stroke-width 9})
      (is (= 9.0 (.getStrokeWidth p))) (is (= Color/RED (.getFill p))))
    (testing "nil clears"
      (s/apply-style! p {:effect nil :stroke "none" :stroke-dasharray nil})
      (is (nil? (.getEffect p))) (is (nil? (.getStroke p))) (is (empty? (.getStrokeDashArray p)))))
  (testing "fill-opacity multiplies into the fill"
    (let [r (Rectangle.)]
      (s/apply-style! r {:fill "#ff0000" :fill-opacity 0.4})
      (is (near? 0.4 (.getOpacity ^Color (.getFill r))))))
  (testing "bad enum value is reported, with the allowed set"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"stroke-linecap" (s/apply-style! (Path.) {:stroke-linecap :wavy})))))
