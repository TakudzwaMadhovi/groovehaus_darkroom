(ns darkroom.imaging.denoise-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.core :as core]
            [darkroom.imaging.denoise :as denoise]
            [darkroom.imaging.pipeline :as pipeline])
  (:import (java.util Random)))

(defn- clamp [v] (max 0 (min 255 (long v))))
(defn- ch [p s] (bit-and (unsigned-bit-shift-right p s) 0xFF))

(defn- scene
  "w x h: smooth gradient with a hard vertical edge at the middle (left dark,
  right light) so we can check that denoising does not smear edges."
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

(defn- psnr [a b]
  (let [^ints x (:pixels a) ^ints y (:pixels b)
        se (loop [i 0 s 0.0]
             (if (< i (alength x))
               (let [p (aget x i) q (aget y i)]
                 (recur (inc i) (+ s (reduce + (for [sh [16 8 0]] (let [d (- (ch p sh) (ch q sh))] (* d d)))))))
               s))]
    (* 10 (Math/log10 (/ (* 255.0 255.0) (/ se (* 3.0 (alength x))))))))

(deftest strength-zero-is-identity
  (let [i (scene 32 32)]
    (is (identical? i (denoise/denoise i 0)))
    (is (identical? i (pipeline/render i pipeline/default-settings)))))

(deftest actually-reduces-noise
  (let [clean (scene 160 120)
        dirty (noisy clean 15)
        base  (psnr clean dirty)]
    (testing "every quality tier improves PSNR, and stronger settings help more"
      (let [p40 (psnr clean (denoise/denoise dirty 40))
            p80 (psnr clean (denoise/denoise dirty 80))]
        (is (> p40 (+ base 1.5)) (str "p40=" p40 " base=" base))
        (is (> p80 (+ p40 3.0)) (str "p80=" p80 " p40=" p40)))
      (doseq [q [:draft :preview :final]]
        (is (> (psnr clean (denoise/denoise dirty 70 {:quality q})) (+ base 4.0)) (str q))))
    (testing "a hard edge stays sharp: pixels either side of it keep their levels"
      (let [out (denoise/denoise dirty 70)
            row 60 w 160
            at (fn [x] (ch (aget ^ints (:pixels out) (+ (* row w) x)) 8))]
        (is (< (at 70) 100))
        (is (> (at 90) 150))
        (is (> (- (at 82) (at 77)) 80))))))

(deftest output-shape-and-purity
  (let [i (assoc (noisy (scene 48 40) 10) :width 48 :height 40)
        before (vec (:pixels i))
        o (denoise/denoise i 50)]
    (is (= [48 40] [(:width o) (:height o)]))
    (is (= (* 48 40) (alength ^ints (:pixels o))))
    (is (= before (vec (:pixels i))) "input untouched")))

(deftest alpha-is-preserved
  (let [i (core/image 24 24 (int-array 576 (unchecked-int 0x80808080)))
        o (denoise/denoise i 60)]
    (is (every? #(= 0x80 (ch % 24)) (:pixels o)))))

(deftest flat-colour-stays-flat
  (let [i (core/image 24 24 (int-array 576 (unchecked-int 0xFF4080C0)))
        o (denoise/denoise i 100)]
    (testing "within 2 levels (OpenCV works in Lab, so a +-1 round trip is expected)"
      (is (every? (fn [p] (every? #(<= (Math/abs (- (ch p %) (ch (unchecked-int 0xFF4080C0) %))) 2) [16 8 0])) (:pixels o))))))

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
        src    (scene 8 8)]
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
        src    (scene 8 8)]
    (with-redefs [pipeline/stages stages pipeline/default-settings {:denoise 0}]
      (let [r (pipeline/renderer src)]
        (r {:denoise 50} {:quality :draft})
        (r {:denoise 50} {:quality :draft})
        (is (= [[:denoise :draft]] @calls) "same quality is cached")
        (r {:denoise 50} {:quality :preview})
        (is (= [[:denoise :draft] [:denoise :preview]] @calls) "higher quality request recomputes")
        (r {:denoise 50} {:quality :draft})
        (is (= 2 (count @calls)) "a cached higher-quality result satisfies a draft request")))))
