(ns darkroom.imaging.detail-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.color :as color]
            [darkroom.imaging.detail :as detail]
            [darkroom.imaging.pipeline :as pipeline]
            [darkroom.imaging.scene :as scene])
  (:import (java.util Random)))

(defn- build
  "Scene image w x h whose pixel at (x, y) is (f x y) -> [r g b] (linear)."
  [w h f]
  (scene/image w h (float-array (for [y (range h) x (range w) c (f x y)] c))))

(defn- at [sc x y c] (double (aget ^floats (:data sc) (+ (* 3 (+ (* y (:width sc)) x)) c))))
(defn- enc [x] (scene/srgb-encode-extended x))
(defn- grey [v] [v v v])

(defn- std
  "Standard deviation of channel 0 over the rectangle."
  [sc x0 y0 x1 y1]
  (let [vs (for [y (range y0 y1) x (range x0 x1)] (at sc x y 0))
        m  (/ (reduce + vs) (count vs))]
    (Math/sqrt (/ (reduce + (map #(* (- % m) (- % m)) vs)) (count vs)))))

(defn- noise-image
  "Mid-grey with +-`amp` noise (encoded-ish), deterministic."
  [w h amp]
  (let [r (Random. 5)]
    (scene/image w h (float-array (mapcat (fn [_] (let [v (+ 0.25 (* amp (.nextGaussian r)))] [v v v])) (range (* w h)))))))

;; --- blur ----------------------------------------------------------------------

(deftest blur-keeps-flat-planes-flat
  (let [p (float-array (repeat (* 40 30) 0.37))]
    (doseq [sigma [0.8 2.0 12.0 30.0]]
      (is (every? #(< (Math/abs (- 0.37 (double %))) 1e-4) (detail/blur-plane p 40 30 sigma)) (str sigma)))
    (is (identical? p (detail/blur-plane p 40 30 0.1)) "negligible sigma returns the input")))

(deftest blur-impulse-is-a-gaussian
  (let [w 41 h 41 p (float-array (* w h))
        _ (aset p (+ (* 20 w) 20) (float 1.0))
        b (detail/blur-plane p w h 2.0)]
    (is (< (Math/abs (- 1.0 (reduce + (map double b)))) 1e-3) "energy is conserved")
    (is (< (Math/abs (- (/ 1.0 (* 2 Math/PI 4.0)) (aget b (+ (* 20 w) 20)))) 0.002) "peak = 1/(2 pi sigma^2)")
    (is (< (Math/abs (- (aget b (+ (* 20 w) 18)) (aget b (+ (* 20 w) 22)))) 1e-6) "symmetric")))

(deftest blur-attenuates-by-the-gaussian-transfer-function
  ;; A sine of wavelength L is attenuated by exp(-2 pi^2 sigma^2 / L^2). Check both
  ;; the direct path (small sigma) and the reduced-copy path (large sigma).
  (doseq [[sigma lam] [[1.0 20.0] [20.0 200.0]]]
    (let [w 400 h 8
          p (float-array (for [_ (range h) x (range w)] (* 0.5 (Math/sin (/ (* 2 Math/PI x) lam)))))
          b (detail/blur-plane p w h sigma)
          amp (apply max (for [x (range 100 300)] (Math/abs (double (aget b (+ (* 4 w) x))))))
          expected (* 0.5 (Math/exp (/ (* -2 Math/PI Math/PI sigma sigma) (* lam lam))))]
      (is (< (Math/abs (- amp expected)) 0.02) (str "sigma " sigma ": " amp " vs " expected)))))

;; --- local contrast -------------------------------------------------------------

(deftest neutral-settings-return-the-same-image
  (let [i (noise-image 16 16 0.05)]
    (is (identical? i (detail/local-contrast i {})))
    (is (identical? i (detail/local-contrast i {:sharpen-radius 2.0 :sharpen-masking 1.0})))
    (is (identical? i (detail/dehaze i {})))
    (is (identical? i (detail/reduce-color-noise i {})))))

(deftest texture-amplifies-or-softens-fine-detail
  (let [i (noise-image 96 96 0.04)
        base (std i 10 10 86 86)]
    (is (> (std (detail/local-contrast i {:texture 1.0}) 10 10 86 86) (* 1.4 base)))
    (is (< (std (detail/local-contrast i {:texture -1.0}) 10 10 86 86) (* 0.7 base)))))

(deftest clarity-works-on-mid-sized-structure-in-the-midtones
  ;; the clarity radius is 1.2% of the long edge: ~7 px on a 600 px image
  (let [edge (build 600 20 (fn [x _] (grey (if (< x 300) 0.15 0.35))))
        out  (detail/local-contrast edge {:clarity 1.0})]
    (testing "halos: the bright side gets brighter and the dark side darker next to the edge"
      (is (> (at out 306 10 0) (+ 0.35 0.008)))
      (is (< (at out 293 10 0) (- 0.15 0.005))))
    (testing "flat areas far from the edge are untouched"
      (is (< (Math/abs (- 0.35 (at out 590 10 0))) 0.002))
      (is (< (Math/abs (- 0.15 (at out 10 10 0))) 0.002))))
  (testing "highlights are protected: clarity does little near white"
    (let [edge (build 600 20 (fn [x _] (grey (if (< x 300) 0.85 1.0))))
          out  (detail/local-contrast edge {:clarity 1.0})]
      (is (< (Math/abs (- 1.0 (at out 306 10 0))) 0.01)))))

(deftest sharpening
  (let [edge (build 80 20 (fn [x _] (grey (if (< x 40) 0.1 0.5))))
        rise (fn [sc] (let [lo (enc 0.1) hi (enc 0.5)]
                        (count (for [x (range 30 50) :let [v (enc (at sc x 10 0))] :when (< (+ lo (* 0.1 (- hi lo))) v (- hi (* 0.1 (- hi lo))))] x))))]
    (testing "a soft edge gets steeper and overshoots"
      (let [soft (build 80 20 (fn [x _] (grey (+ 0.1 (* 0.4 (/ (Math/tanh (/ (- x 40) 3.0)) 2.0)) 0.2))))
            out  (detail/local-contrast soft {:sharpen 80 :sharpen-radius 1.5})]
        (is (< (rise out) (rise soft)) "fewer pixels in the transition")
        (is (> (at out 43 10 0) (at soft 43 10 0)))
        (is (< (at out 37 10 0) (at soft 37 10 0)))))
    (testing "flat regions are unchanged"
      (let [out (detail/local-contrast edge {:sharpen 100 :sharpen-radius 1.0})]
        (is (< (Math/abs (- 0.1 (at out 5 10 0))) 1e-3))
        (is (< (Math/abs (- 0.5 (at out 75 10 0))) 1e-3))))
    (testing "colour is untouched: channel differences (encoded) survive"
      (let [col (build 80 20 (fn [x _] (if (< x 40) [0.1 0.3 0.05] [0.5 0.2 0.4])))
            out (detail/local-contrast col {:sharpen 100})]
        (doseq [x [38 41 60]]
          (is (< (Math/abs (- (- (enc (at col x 10 0)) (enc (at col x 10 1))) (- (enc (at out x 10 0)) (enc (at out x 10 1))))) 5e-3)))))
    (testing "masking restricts sharpening to real edges: noise is amplified less"
      (let [n (noise-image 64 64 0.01)
            plain  (std (detail/local-contrast n {:sharpen 100 :sharpen-masking 0.0}) 8 8 56 56)
            masked (std (detail/local-contrast n {:sharpen 100 :sharpen-masking 1.0}) 8 8 56 56)]
        (is (< masked (* 0.8 plain)))))
    (testing "the radius is in full-size pixels: a small :scale softens the effect"
      (let [soft (build 80 20 (fn [x _] (grey (+ 0.2 (* 0.2 (Math/tanh (/ (- x 40) 3.0)))))))
            strong (detail/local-contrast soft {:sharpen 100 :sharpen-radius 2.0} {:scale 1.0})
            weak   (detail/local-contrast soft {:sharpen 100 :sharpen-radius 2.0} {:scale 0.2})]
        (is (> (Math/abs (- (at strong 44 10 0) (at soft 44 10 0)))
               (Math/abs (- (at weak 44 10 0) (at soft 44 10 0)))))))))

(deftest over-range-highlights-survive
  (let [img (build 40 10 (fn [x _] (grey (if (< x 20) 0.3 3.0))))
        out (detail/local-contrast img {:texture 0.5 :clarity 0.5 :sharpen 50})]
    (is (> (at out 35 5 0) 2.0) "headroom above 1.0 is kept")))

;; --- colour noise ---------------------------------------------------------------

(deftest colour-noise-reduction
  (let [r (Random. 9)
        base (fn [x] (if (< x 32) [0.30 0.30 0.30] [0.30 0.30 0.30]))
        noisy (build 64 64 (fn [x _] (let [[g1 g2 g3] (base x)
                                           n (fn [] (* 0.04 (.nextGaussian r)))]
                                       ;; chroma noise: red and blue wander, green moves the other way
                                       (let [a (n) b (n)] [(+ g1 a) (- g2 (* 0.5 (+ a b))) (+ g3 b)]))))
        chroma-std (fn [sc] (let [vs (for [y (range 8 56) x (range 8 56)] (- (enc (at sc x y 0)) (enc (at sc x y 2))))
                                  m (/ (reduce + vs) (count vs))]
                              (Math/sqrt (/ (reduce + (map #(* (- % m) (- % m)) vs)) (count vs)))))
        luma (fn [sc x y] (let [lw (color/luma-weights :working)]
                            (reduce + (map-indexed (fn [c w] (* w (enc (at sc x y c)))) lw))))
        out  (detail/reduce-color-noise noisy {:denoise-color 100})]
    (is (< (chroma-std out) (* 0.5 (chroma-std noisy))) "colour speckle is halved at least")
    (testing "brightness is preserved"
      (is (< (/ (reduce + (for [y (range 8 56) x (range 8 56)] (Math/abs (- (luma out x y) (luma noisy x y))))) (* 48 48)) 0.01)))
    (is (= [64 64] [(:width out) (:height out)])))
  (testing "real colour edges are kept"
    (let [edge (build 64 32 (fn [x _] (if (< x 32) [0.5 0.1 0.1] [0.1 0.1 0.5])))
          out  (detail/reduce-color-noise edge {:denoise-color 100})
          d    (fn [sc x] (- (enc (at sc x 16 0)) (enc (at sc x 16 2))))]
      (is (> (d out 28) 0.25)) (is (< (d out 36) -0.25)))))

;; --- dehaze ---------------------------------------------------------------------

(deftest dehaze-removes-and-adds-haze
  (let [r (Random. 3)
        ;; a textured scene with a few very dark spots, and a bright "sky" band so the
        ;; dark-channel airlight estimate has something hazy to find
        clean (build 120 90 (fn [x y] (if (< y 8)
                                        [0.9 0.9 0.9]
                                        (let [v (+ 0.03 (* 0.5 (Math/abs (Math/sin (/ (+ (* x 0.21) (* y 0.13)) 1.0)))))
                                              j (fn [] (+ v (* 0.05 (.nextDouble r))))]
                                          [(j) (j) (j)]))))
        t 0.6 a 0.85
        hazy (scene/image 120 90 (float-array (map #(+ (* t (double %)) (* a (- 1.0 t))) (:data clean))))
        sd   (fn [sc] (std sc 0 0 120 90))
        mae  (fn [x y] (/ (reduce + (map #(Math/abs (- (double %1) (double %2))) (:data x) (:data y))) (alength ^floats (:data x))))]
    (testing "haze lowers contrast; dehaze brings it back toward the clean image"
      (is (< (sd hazy) (* 0.8 (sd clean))))
      (let [out (detail/dehaze hazy {:dehaze 1.0})]
        (is (> (sd out) (* 1.2 (sd hazy))))
        (is (< (mae out clean) (* 0.7 (mae hazy clean))))))
    (testing "a negative amount adds haze: contrast drops, blacks lift"
      (let [out (detail/dehaze clean {:dehaze -1.0})]
        (is (< (sd out) (* 0.8 (sd clean))))
        (is (> (apply min (:data out)) (+ 0.1 (apply min (:data clean)))))))
    (testing "a clear scene is barely changed by a mild dehaze"
      (is (< (mae (detail/dehaze clean {:dehaze 0.1}) clean) 0.05)))
    (testing "shape is kept"
      (is (= [120 90 (* 3 120 90)] (let [o (detail/dehaze hazy {:dehaze 0.5})] [(:width o) (:height o) (alength ^floats (:data o))]))))))

;; --- pipeline -------------------------------------------------------------------

(deftest pipeline-runs-the-detail-stages
  (let [i (noise-image 64 48 0.03)
        o (pipeline/render i {:sharpen 60 :clarity 0.4 :texture 0.3 :dehaze 0.2 :denoise-color 30 :exposure 0.1})]
    (is (= [64 48] [(:width o) (:height o)]))
    (is (not= (vec (:data i)) (vec (:data o)))))
  (doseq [k [:texture :clarity :dehaze :sharpen :sharpen-radius :sharpen-masking :denoise-color]]
    (is (contains? pipeline/default-settings k) (str k))))

(deftest renderer-reruns-scaled-stages-when-the-scale-changes
  (let [i (noise-image 32 32 0.03) r (pipeline/renderer i)
        a (r {:sharpen 80 :sharpen-radius 2.0} {:scale 1.0})
        b (r {:sharpen 80 :sharpen-radius 2.0} {:scale 0.2})]
    (is (not= (vec (:data a)) (vec (:data b))))))
