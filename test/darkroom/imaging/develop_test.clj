(ns darkroom.imaging.develop-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.core :as core]
            [darkroom.imaging.develop :as develop]))

(defn- ch [p s] (bit-and (unsigned-bit-shift-right p s) 0xFF))
(defn- px [r g b] (unchecked-int (bit-or 0xFF000000 (bit-shift-left r 16) (bit-shift-left g 8) b)))
(defn- grid [img]
  (let [{:keys [width height pixels]} img]
    (vec (for [y (range height)] (vec (for [x (range width)] (aget ^ints pixels (+ (* y width) x))))))))

;; --- parity with the design handoff's JS engine -------------------------------

(def ^:private parity (edn/read-string (slurp (io/resource "resources/develop_parity.edn"))))

(defn- rgb->image [w h flat]
  (core/image w h (int-array (map (fn [[r g b]] (px r g b)) (partition 3 flat)))))

(deftest tone-matches-the-reference-engine
  (let [{:keys [width height input cases]} parity
        src (rgb->image width height input)]
    (doseq [{:keys [name settings output]} cases]
      (testing name
        (let [out    (develop/tone src settings)
              expect (partition 3 output)
              diffs  (for [[p [r g b]] (map vector (:pixels out) expect)
                           d [(- (ch p 16) r) (- (ch p 8) g) (- (ch p 0) b)]]
                       (Math/abs (long d)))]
          (is (= (* width height) (alength ^ints (:pixels out))))
          (is (every? #(= 255 (ch % 24)) (:pixels out)) "alpha kept opaque")
          (is (<= (apply max diffs) 1) (str name ": max channel diff " (apply max diffs)))
          (is (>= (/ (count (filter zero? diffs)) (double (count diffs))) 0.99)
              (str name ": " (count (remove zero? diffs)) " of " (count diffs) " channels differ")))))))

(deftest tone-neutral
  (is (develop/tone-neutral? {}))
  (is (develop/tone-neutral? develop/defaults))
  (is (not (develop/tone-neutral? {:grain 0.1})))
  (is (not (develop/tone-neutral? {:curve [0 0.3 0.5 0.75 1]}))))

(deftest tone-effects
  (let [grey (core/image 4 4 (int-array 16 (px 128 128 128)))
        mid  (fn [img] (ch (aget ^ints (:pixels img) 5) 8))]
    (is (> (mid (develop/tone grey {:exposure 1.0})) 128) "+1 stop brightens")
    (is (< (mid (develop/tone grey {:exposure -1.0})) 128) "-1 stop darkens")
    (testing "temperature warms: red up, blue down"
      (let [p (aget ^ints (:pixels (develop/tone grey {:temp 0.8})) 5)]
        (is (> (ch p 16) (ch p 0)))))
    (testing "black & white removes colour"
      (let [col (core/image 2 2 (int-array 4 (px 200 100 40)))
            p (aget ^ints (:pixels (develop/tone col {:bw 1.0})) 0)]
        (is (= (ch p 16) (ch p 8) (ch p 0)))))
    (testing "vignette darkens corners more than the centre"
      (let [big (core/image 40 40 (int-array 1600 (px 200 200 200)))
            out (develop/tone big {:vignette 1.0})
            at (fn [x y] (ch (aget ^ints (:pixels out) (+ (* y 40) x)) 8))]
        (is (< (at 0 0) (at 20 20)))))
    (testing "grain is deterministic"
      (is (= (grid (develop/tone grey {:grain 0.8})) (grid (develop/tone grey {:grain 0.8})))))))

;; --- curve -----------------------------------------------------------------

(deftest curve-lut
  (let [id (develop/curve-lut develop/default-curve)]
    (is (= 256 (alength id)))
    (is (every? #(< (Math/abs (- (aget id %) (/ % 255.0))) 1e-9) (range 256)) "identity points -> identity curve"))
  (let [lut (develop/curve-lut [0 0.1 0.5 0.9 1])]
    (is (< (aget lut 64) 0.25) "crushed shadows")
    (is (> (aget lut 192) 0.75) "lifted highlights")
    (is (every? #(<= 0.0 (aget lut %) 1.0) (range 256)))
    (is (= [0.0 1.0] [(aget lut 0) (aget lut 255)]))))

;; --- geometry ----------------------------------------------------------------

(def ^:private src23 (core/image 3 2 (int-array [1 2 3 4 5 6])))

(deftest geometry-neutral-returns-same
  (is (identical? src23 (develop/geometry src23 {})))
  (is (identical? src23 (develop/geometry src23 {:angle 0.0 :aspect "orig" :flip false}))))

(deftest geometry-flip-and-crop
  (is (= [[3 2 1] [6 5 4]] (grid (develop/geometry src23 {:flip true}))) "mirror")
  (let [wide (core/image 4 2 (int-array [1 2 3 4 5 6 7 8]))
        sq   (develop/geometry wide {:aspect "1:1"})]
    (is (= [2 2] [(:width sq) (:height sq)]))
    (is (= [[2 3] [6 7]] (grid sq)) "centred crop keeps the middle columns"))
  (let [out (develop/geometry (core/image 160 90 (int-array (* 160 90) 7)) {:aspect "4:5"})]
    (is (= [72 90] [(:width out) (:height out)]) "4:5 of a 160x90 frame")))

(deftest geometry-straighten
  (let [uni (core/image 80 60 (int-array 4800 (px 90 120 150)))
        out (develop/geometry uni {:angle 7.5})]
    (is (= [80 60] [(:width out) (:height out)]))
    (is (every? #(= (px 90 120 150) %) (:pixels out)) "zoom keeps the frame filled: no edge colour leaks in"))
  (let [grad (core/image 100 100 (int-array (for [y (range 100) x (range 100)] (px (* 2 x) 0 0))))
        out  (develop/geometry grad {:angle 10.0})
        mid  #(ch (aget ^ints (:pixels %) (+ (* 50 100) 50)) 16)]
    (is (< (Math/abs (- (mid grad) (mid out))) 4) "centre pixel stays put")))

;; --- resize --------------------------------------------------------------------

(deftest resize-long-edge
  (let [img (core/image 100 50 (int-array 5000 (px 10 20 30)))]
    (let [o (develop/resize-long-edge img 20)]
      (is (= [20 10] [(:width o) (:height o)]))
      (is (every? #(= (px 10 20 30) %) (:pixels o))))
    (is (identical? img (develop/resize-long-edge img 500)) "never enlarges")
    (is (identical? img (develop/resize-long-edge img 0)) "0 = full resolution")
    (is (identical? img (develop/resize-long-edge img nil)))
    (let [o (develop/resize-long-edge (core/image 1000 400 (int-array 400000 (px 1 2 3))) 100)]
      (is (= [100 40] [(:width o) (:height o)])))))
