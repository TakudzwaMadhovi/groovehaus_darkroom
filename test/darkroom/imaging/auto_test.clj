(ns darkroom.imaging.auto-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.auto :as auto]
            [darkroom.imaging.color :as color]
            [darkroom.imaging.develop :as develop]
            [darkroom.imaging.scene :as scene])
  (:import (java.util Random)))

(defn- build [w h f] (scene/image w h (float-array (for [y (range h) x (range w) c (f x y)] c))))

(defn- textured
  "A scene of random grey values drawn between lo and hi (log-uniform), optionally
  multiplied by per-channel gains."
  ([lo hi] (textured lo hi [1.0 1.0 1.0]))
  ([lo hi gains]
   (let [r (Random. 7) k (Math/log (/ hi lo))]
     (build 90 70 (fn [_ _] (let [v (* lo (Math/exp (* k (.nextDouble r))))] (mapv #(* v %) gains)))))))

(defn- median-enc [sc]
  (let [ls (sort (for [i (range (quot (alength ^floats (:data sc)) 3))]
                   (let [[lr lg lb] (color/luma-weights :working) d (:data sc) j (* 3 i)]
                     (+ (* lr (aget ^floats d j)) (* lg (aget ^floats d (+ j 1))) (* lb (aget ^floats d (+ j 2)))))))]
    (scene/srgb-encode-extended (nth ls (quot (count ls) 2)))))

(deftest sampling
  (let [big (build 400 300 (fn [x y] [(/ x 400.0) (/ y 300.0) 0.5]))
        s (auto/sample big 10000)]
    (is (<= (* (:width s) (:height s)) 12000))
    (is (> (* (:width s) (:height s)) 5000))
    (is (= (* 3 (:width s) (:height s)) (alength ^floats (:data s)))))
  (let [small (build 10 10 (fn [_ _] [0.1 0.1 0.1]))]
    (is (identical? small (auto/sample small 10000)))))

;; --- white balance -------------------------------------------------------------

(deftest wb-from-color-inverts-the-sliders
  ;; Take an sRGB-neutral grey, undo the balance a (temp, tint) pair would apply, and
  ;; ask for the balance back: we must recover that pair.
  (let [grey (color/mat-vec (color/convert-matrix :srgb :working) [0.3 0.3 0.3])]
    (doseq [[t n] [[0.0 0.0] [0.4 -0.2] [-0.5 0.3] [0.7 0.5] [-0.3 -0.6]]]
      (let [c (color/mat-vec (color/mat-inv (color/wb-matrix t n)) grey)
            r (auto/wb-from-color c)]
        (is (< (Math/abs (- t (:temp r))) 0.02) (str [t n] " temp " (:temp r)))
        (is (< (Math/abs (- n (:tint r))) 0.02) (str [t n] " tint " (:tint r)))))))

(deftest wb-from-color-stays-in-range
  (let [r (auto/wb-from-color [0.01 0.02 0.9])]
    (is (<= -1.0 (:temp r) 1.0)) (is (<= -1.0 (:tint r) 1.0))))

(deftest average-colour-window
  (let [img (build 10 10 (fn [x y] [(double x) (double y) 1.0]))]
    (is (= [4.0 5.0 1.0] (auto/average-color img 4 5 3)) "a 3x3 window around (4,5) averages to its centre")
    (is (= [0.5 0.5 1.0] (auto/average-color img 0 0 3)) "clamped at the corner: (0..1)x(0..1)")))

(deftest auto-wb-neutralises-a-colour-cast
  (let [warm (textured 0.05 0.6 [1.2 1.0 0.85])
        r (auto/auto-wb warm)
        out (develop/tone warm r)
        sums (reduce (fn [acc i] (mapv + acc (map #(aget ^floats (:data out) (+ (* 3 i) %)) [0 1 2])))
                     [0.0 0.0 0.0] (range (* 90 70)))
        [sr sg sb] (mapv #(/ % (* 90 70)) sums)]
    (is (< (:temp r) -0.1) "a warm cast needs cooling")
    (testing "the balanced picture is close to neutral in sRGB"
      (let [m (color/convert-matrix :working :srgb)
            [r' g' b'] (color/mat-vec m [sr sg sb])]
        (is (< (Math/abs (- 1.0 (/ r' g'))) 0.08)) (is (< (Math/abs (- 1.0 (/ b' g'))) 0.08))))))

(deftest auto-wb-ignores-large-saturated-areas
  ;; 70% lawn: plain grey-world would call the picture magenta; the neutral pixels should win
  (let [r (Random. 3)
        sc (build 100 80 (fn [x y] (if (< (+ (* 3 y) x) 90) [0.3 0.3 0.3]
                                       (if (< (.nextDouble r) 0.75) [0.05 0.35 0.05] [0.25 0.25 0.25]))))
        w (auto/auto-wb (scene/image 100 80 (:data sc)))]
    (is (< (Math/abs (:temp w)) 0.15)) (is (< (Math/abs (:tint w)) 0.15) (str w))))

;; --- tone -----------------------------------------------------------------------

(deftest auto-tone-aims-the-median-at-mid-grey
  (doseq [[nm sc] [["underexposed" (textured 0.01 0.2)]
                   ["normal" (textured 0.01 0.9)]
                   ["a little dark" (textured 0.01 0.25)]]]
    (testing nm
      (let [s (auto/auto-tone sc)
            out (develop/tone sc s)]
        (is (< (Math/abs (- 0.46 (median-enc out))) 0.05) (str s " median " (median-enc out)))))))

(deftest auto-tone-directions
  (let [dark (auto/auto-tone (textured 0.002 0.08))
        bright (auto/auto-tone (textured 0.4 3.0))
        normal (auto/auto-tone (textured 0.01 0.9))]
    (is (> (:exposure dark) 1.0) "a dark picture is brightened")
    (is (> (:shadows dark) -0.01))
    (is (< (:exposure bright) -1.0) "a bright picture is darkened")
    (is (< (Math/abs (:exposure normal)) 1.0))))

(deftest auto-tone-cannot-fix-what-the-sliders-cannot-reach
  (let [s (auto/auto-tone (textured 0.002 0.08))]
    (is (= 2.0 (:exposure s)) "+3.8 EV is needed; the slider stops at +2")
    (is (> (median-enc (develop/tone (textured 0.002 0.08) s)) 0.25) "but it still gets most of the way")))

(deftest auto-tone-recovers-highlights-it-cannot-expose-away
  ;; mostly dark, with a patch of bright sky: lifting the median would clip the sky,
  ;; so exposure is limited and the highlights slider takes up the slack
  (let [r (Random. 11)
        sc (build 100 100 (fn [x y] (let [v (if (< y 10) (+ 2.0 (.nextDouble r)) (* 0.02 (+ 1.0 (* 3.0 (.nextDouble r)))))] [v v v])))
        s (auto/auto-tone sc)]
    (is (< (:highlights s) -0.2) (str s))
    (is (> (:exposure s) 0.0))))

(deftest auto-tone-stays-in-the-sliders-ranges
  (doseq [sc [(textured 0.0005 0.02) (textured 0.5 6.0) (textured 0.001 8.0) (build 40 40 (fn [_ _] [0.2 0.2 0.2]))]]
    (let [s (auto/auto-tone sc)]
      (is (<= -2.0 (:exposure s) 2.0))
      (doseq [k [:contrast :highlights :shadows :whites :blacks]]
        (is (<= -1.0 (k s) 1.0) (str k " " (k s))))
      (is (= #{:exposure :contrast :highlights :shadows :whites :blacks} (set (keys s)))))))

(deftest auto-tone-looks-through-the-white-balance
  ;; A strong cast changes how bright the picture really is once balanced: the result
  ;; must aim the median right when the balance is applied alongside it.
  (let [sc (textured 0.01 0.5 [1.4 1.0 0.6])
        wb {:temp -0.6 :tint 0.1}
        s  (auto/auto-tone sc wb)]
    (is (= #{:exposure :contrast :highlights :shadows :whites :blacks} (set (keys s))))
    (is (< (Math/abs (- 0.46 (median-enc (develop/tone sc (merge wb s))))) 0.05))))
