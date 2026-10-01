(ns darkroom.imaging.denoise-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.core :as core]
            [darkroom.imaging.denoise :as denoise]
            [darkroom.imaging.pipeline :as pipeline]
            [darkroom.imaging.scene :as scene])
  (:import (java.util Random)))

(defn- clamp [v] (max 0 (min 255 (long v))))
(defn- ch [p s] (bit-and (unsigned-bit-shift-right p s) 0xFF))
(defn- argb [sc] (:pixels (scene/->argb sc)))

(defn- test-image
  "w x h: smooth gradient with a hard vertical edge at the middle (left dark,
  right light) so we can check that denoising does not smear edges. Returns
  an 8-bit image (see `noisy`, `->scene`)."
  [w h]
  (let [px (int-array (* w h))]
    (dotimes [y h]
      (dotimes [x w]
        (let [base (if (< x (quot w 2)) 60 190)
              v    (+ base (quot (* 20 y) h))]
          (aset px (+ (* y w) x) (unchecked-int (bit-or 0xFF000000 (bit-shift-left v 16) (bit-shift-left v 8) v))))))
    (core/image w h px)))

(defn- noisy [img sigma]
  (let [rnd (Random. 11) ^ints src (:pixels img) out (int-array (alength src))]
    (dotimes [i (alength src)]
      (let [p (aget src i)
            c (fn [s] (clamp (Math/round (+ (ch p s) (* sigma (.nextGaussian rnd))))))]
        (aset out i (unchecked-int (bit-or (bit-and p 0xFF000000) (bit-shift-left (c 16) 16) (bit-shift-left (c 8) 8) (c 0))))))
    (assoc img :pixels out)))

(defn- psnr
  "PSNR between two 8-bit images, or between an 8-bit image and a scene image."
  [a b]
  (let [^ints x (:pixels a) ^ints y (if (:pixels b) (:pixels b) (argb b))
        se (loop [i 0 s 0.0]
             (if (< i (alength x))
               (let [p (aget x i) q (aget y i)]
                 (recur (inc i) (+ s (reduce + (for [sh [16 8 0]] (let [d (- (ch p sh) (ch q sh))] (* d d)))))))
               s))]
    (* 10 (Math/log10 (/ (* 255.0 255.0) (/ se (* 3.0 (alength x))))))))

(deftest strength-zero-is-identity
  (let [i (scene/from-argb (test-image 32 32))]
    (is (identical? i (denoise/denoise i 0)))
    (is (identical? i (pipeline/render i pipeline/default-settings)))))

(deftest actually-reduces-noise
  (let [clean (test-image 160 120)
        dirty (noisy clean 15)
        dirty-s (scene/from-argb dirty)
        base  (psnr clean dirty)]
    (testing "every quality tier improves PSNR, and stronger settings help more"
      (let [p40 (psnr clean (denoise/denoise dirty-s 40))
            p80 (psnr clean (denoise/denoise dirty-s 80))]
        (is (> p40 (+ base 1.5)) (str "p40=" p40 " base=" base))
        (is (> p80 (+ p40 3.0)) (str "p80=" p80 " p40=" p40)))
      (doseq [q [:draft :preview :final]]
        (is (> (psnr clean (denoise/denoise dirty-s 70 {:quality q})) (+ base 4.0)) (str q))))
    (testing "a hard edge stays sharp: pixels either side of it keep their levels"
      (let [out (argb (denoise/denoise dirty-s 70))
            row 60 w 160
            at (fn [x] (ch (aget ^ints out (+ (* row w) x)) 8))]
        (is (< (at 70) 100))
        (is (> (at 90) 150))
        (is (> (- (at 82) (at 77)) 80))))))

(deftest output-shape-and-purity
  (let [i (scene/from-argb (noisy (test-image 48 40) 10))
        before (vec (:data i))
        o (denoise/denoise i 50)]
    (is (= [48 40] [(:width o) (:height o)]))
    (is (= (* 3 48 40) (alength ^floats (:data o))))
    (is (= before (vec (:data i))) "input untouched")))

(deftest flat-colour-stays-flat
  (let [c (unchecked-int 0xFF4080C0)
        i (scene/from-argb (core/image 24 24 (int-array 576 c)))
        o (argb (denoise/denoise i 100))]
    (testing "within 1 level"
      (is (every? (fn [p] (every? #(<= (Math/abs (- (ch p %) (ch c %))) 1) [16 8 0])) o)))))

(deftest over-range-values-are-left-alone
  ;; Headroom above 1.0 (and out-of-gamut negatives) cannot go through the
  ;; 16-bit denoiser; those samples must come back exactly as they were.
  (let [n (* 32 32)
        data (float-array (mapcat (fn [i] (cond (< (rem i 32) 8) [2.5 3.0 4.0]
                                                (< (rem i 32) 12) [-0.2 0.5 0.5]
                                                :else [0.3 0.3 0.3]))
                                  (range n)))
        o (denoise/denoise (scene/image 32 32 data) 100)]
    (is (every? true? (for [i (range n) :let [x (rem i 32)] :when (< x 12)
                            c (range (if (< x 8) 3 1))] ; x >= 8: only channel 0 is out of range
                        (= (aget ^floats data (+ (* 3 i) c)) (aget ^floats (:data o) (+ (* 3 i) c))))))))

;; --- pipeline: stage caching ------------------------------------------------

(defn- spy-stage [calls id ks & [{:keys [quality?]}]]
  {:id id :keys ks :quality? quality?
   :neutral? (fn [s] (every? #(zero? (long (get s % 0))) ks))
   :op (fn [img s opts] (swap! calls conj (if quality? [id (:quality opts)] id)) img)})

(deftest renderer-skips-unchanged-stages
  (let [calls  (atom [])
        stages [(spy-stage calls :denoise [:denoise])
                (spy-stage calls :geometry [:angle])
                (spy-stage calls :tone [:exposure :contrast])]
        src    (scene/image 8 8 (float-array 192))]
    (with-redefs [pipeline/stages stages
                  pipeline/default-settings {:denoise 0 :angle 0 :exposure 0 :contrast 0}]
      (let [r (pipeline/renderer src)]
        (r {:denoise 50 :angle 2 :exposure 1 :contrast 5})
        (is (= [:denoise :geometry :tone] @calls) "first render runs everything")
        (reset! calls [])
        (r {:denoise 50 :angle 2 :exposure 2 :contrast 5})
        (is (= [:tone] @calls) "a tone change must not re-run denoise or geometry")
        (reset! calls [])
        (r {:denoise 50 :angle 3 :exposure 2 :contrast 5})
        (is (= [:geometry :tone] @calls) "a geometry change re-runs geometry and tone only")
        (reset! calls [])
        (r {:denoise 50 :angle 3 :exposure 2 :contrast 5})
        (is (= [] @calls) "identical request is fully cached")
        (r {:denoise 60 :angle 3 :exposure 2 :contrast 5})
        (is (= [:denoise :geometry :tone] @calls) "changing denoise re-runs everything after it")))))

(deftest renderer-quality-caching
  (let [calls  (atom [])
        stages [(spy-stage calls :denoise [:denoise] {:quality? true})]
        src    (scene/image 8 8 (float-array 192))]
    (with-redefs [pipeline/stages stages pipeline/default-settings {:denoise 0}]
      (let [r (pipeline/renderer src)]
        (r {:denoise 50} {:quality :draft})
        (r {:denoise 50} {:quality :draft})
        (is (= [[:denoise :draft]] @calls) "same quality is cached")
        (r {:denoise 50} {:quality :preview})
        (is (= [[:denoise :draft] [:denoise :preview]] @calls) "higher quality request recomputes")
        (r {:denoise 50} {:quality :draft})
        (is (= 2 (count @calls)) "a cached higher-quality result satisfies a draft request")))))
