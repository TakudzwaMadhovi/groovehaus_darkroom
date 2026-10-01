(ns darkroom.imaging.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.core :as core]
            [darkroom.imaging.pipeline :as pipeline]))

(defn- px [a r g b] (unchecked-int (bit-or (bit-shift-left a 24) (bit-shift-left r 16) (bit-shift-left g 8) b)))
(defn- img [& ps] (core/image (count ps) 1 (int-array ps)))
(defn- first-px [i] (aget ^ints (:pixels i) 0))

(deftest brightness
  (testing "zero is identity"
    (is (= (px 255 10 20 30) (first-px (core/adjust-brightness (img (px 255 10 20 30)) 0)))))
  (testing "positive adds, preserves alpha"
    (is (= (px 128 110 120 130)
           (first-px (core/adjust-brightness (img (px 128 10 20 30)) 39.2157)))))
  (testing "clamps to 0..255"
    (is (= (px 255 255 255 255) (first-px (core/adjust-brightness (img (px 255 250 250 250)) 100))))
    (is (= (px 255 0 0 0) (first-px (core/adjust-brightness (img (px 255 5 5 5)) -100)))))
  (testing "input is not mutated"
    (let [i (img (px 255 10 10 10))]
      (core/adjust-brightness i 50)
      (is (= (px 255 10 10 10) (first-px i))))))

(deftest pipeline-render
  (let [i (img (px 255 100 100 100))]
    (is (identical? i (pipeline/render i pipeline/default-settings)))
    (is (not= (first-px i) (first-px (pipeline/render i {:brightness 20}))))))
