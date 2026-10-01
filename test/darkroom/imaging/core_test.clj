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

(deftest fit-downscale
  (let [big (core/image 4 4 (int-array (repeat 16 (px 255 100 50 10))))
        small (core/fit big 2)]
    (is (= [2 2] [(:width small) (:height small)]))
    (is (= (px 255 100 50 10) (first-px small))))
  (let [i (img (px 255 1 2 3))]
    (is (identical? i (core/fit i 10)))))

(deftest brightness-parallel-matches-serial
  ;; 300k pixels crosses the threshold where work is split across threads.
  (let [n   300000
        ps  (int-array (map #(unchecked-int (bit-or 0xFF000000 (* % 7919))) (range n)))
        out ^ints (:pixels (core/adjust-brightness (core/image n 1 ps) 10))
        ch  (fn [p s] (bit-and (unsigned-bit-shift-right p s) 0xFF))
        ref (fn [p] (reduce (fn [acc s] (bit-or acc (bit-shift-left (min 255 (+ 26 (ch p s))) s)))
                            0xFF000000 [16 8 0]))]
    (is (every? #(= (unchecked-int (ref (aget ps %))) (aget out %)) (range n)))))

(defn- ch [p s] (bit-and (unsigned-bit-shift-right p s) 0xFF))

(deftest contrast
  (let [i (img (px 255 128 128 128) (px 255 200 100 50))]
    (testing "mid-grey is fixed, others spread"
      (let [o (:pixels (core/adjust-contrast i 50))]
        (is (= 128 (ch (aget ^ints o 0) 16)))
        (is (> (ch (aget ^ints o 1) 16) 200))
        (is (< (ch (aget ^ints o 1) 0) 50))))
    (testing "negative flattens toward grey"
      (let [o (:pixels (core/adjust-contrast i -50))]
        (is (< (ch (aget ^ints o 1) 16) 200))
        (is (> (ch (aget ^ints o 1) 0) 50))))))

(deftest gamma
  (let [i (img (px 255 0 128 255))
        up (first-px (core/adjust-gamma i 2.0))
        dn (first-px (core/adjust-gamma i 0.5))]
    (testing "endpoints fixed"
      (is (= [0 255] [(ch up 16) (ch up 0)] [(ch dn 16) (ch dn 0)])))
    (testing "gamma>1 brightens midtones, <1 darkens"
      (is (= 181 (ch up 8)))   ; 255*(128/255)^(1/2)
      (is (= 64 (ch dn 8))))))

(deftest saturation
  (let [i (img (px 200 200 100 50))]
    (testing "-100 gives grey, alpha kept"
      (let [p (first-px (core/adjust-saturation i -100))]
        (is (= (ch p 16) (ch p 8) (ch p 0)))
        (is (= 200 (ch p 24)))))
    (testing "greys are unaffected by saturation"
      (let [g (px 255 90 90 90)]
        (is (= g (first-px (core/adjust-saturation (img g) 100))))))
    (testing "positive increases channel spread"
      (let [p (first-px (core/adjust-saturation (img (px 255 200 100 50)) 50))]
        (is (> (- (ch p 16) (ch p 0)) 150))))))

(deftest pipeline-all-settings
  (let [i (img (px 255 100 150 200))
        o (pipeline/render i {:brightness 10 :contrast 20 :gamma 1.5 :saturation -30})]
    (is (not= (first-px i) (first-px o)))
    (is (identical? i (pipeline/render i {:gamma 1.0 :contrast 0})))))
