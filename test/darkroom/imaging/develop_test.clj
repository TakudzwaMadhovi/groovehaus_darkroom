(ns darkroom.imaging.develop-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.core :as core]
            [darkroom.imaging.develop :as develop]
            [darkroom.imaging.scene :as scene]))

(defn- ch [p s] (bit-and (unsigned-bit-shift-right p s) 0xFF))
(defn- px [r g b] (unchecked-int (bit-or 0xFF000000 (bit-shift-left r 16) (bit-shift-left g 8) b)))

(defn- from-rgb
  "Scene image from a seq of [r g b] 8-bit sRGB triples."
  [w h rgbs]
  (scene/from-argb (core/image w h (int-array (map (fn [[r g b]] (px r g b)) rgbs)))))

(defn- uniform [w h [r g b]] (from-rgb w h (repeat (* w h) [r g b])))

(defn- display
  "Packed ARGB ints of a scene image as the user sees it (sRGB)."
  [sc] (:pixels (scene/->argb sc)))

(defn- ch0 [sc i] (ch (aget ^ints (display sc) i) 16))
(defn- lin
  "Channel `c` (0-2) of pixel `i` of a scene image, as linear light."
  [sc i c] (double (aget ^floats (:data sc) (+ (* 3 i) c))))

(defn- grey-scene
  "w x h scene of neutral greys with the given linear values."
  [w h vs]
  (scene/image w h (float-array (mapcat (fn [v] [v v v]) vs))))

(defn- grid
  "Channel 0 of a scene image as rows of floats rounded to 4 places."
  [sc]
  (vec (for [y (range (:height sc))]
         (vec (for [x (range (:width sc))]
                (/ (Math/round (* 1e4 (lin sc (+ (* y (:width sc)) x) 0))) 1e4))))))

;; --- tone --------------------------------------------------------------------

(deftest tone-neutral
  (is (develop/tone-neutral? {}))
  (is (develop/tone-neutral? develop/defaults))
  (is (not (develop/tone-neutral? {:grain 0.1})))
  (is (not (develop/tone-neutral? {:tint 0.2})))
  (is (not (develop/tone-neutral? {:curve [0 0.3 0.5 0.75 1]}))))

(deftest default-tone-is-an-identity
  (let [cols (for [r (range 0 256 51) g (range 0 256 51) b (range 0 256 51)] [r g b])
        greys (for [v (range 256)] [v v v])
        all   (concat cols greys)
        src   (from-rgb (count all) 1 all)
        out   (develop/tone src {})
        diffs (for [[a b] (map vector (display src) (display out))
                    s [16 8 0]]
                (Math/abs (- (ch a s) (ch b s))))]
    (is (<= (apply max diffs) 1))
    (is (>= (/ (count (filter zero? diffs)) (double (count diffs))) 0.98))))

(deftest exposure-acts-in-linear-light
  (let [src (grey-scene 3 1 [0.05 0.1 0.4])]
    (testing "+1 stop doubles the light, -1 stop halves it"
      (let [up (develop/tone src {:exposure 1.0}) dn (develop/tone src {:exposure -1.0})]
        (doseq [i (range 3) c (range 3)]
          (let [x (lin src i c)]
            (is (< (Math/abs (- (* 2.0 x) (lin up i c))) (* 0.002 (* 2.0 x))) "double")
            (is (< (Math/abs (- (* 0.5 x) (lin dn i c))) (* 0.002 x)) "half")))))
    (testing "light pushed past white clips to white"
      (let [out (develop/tone (grey-scene 1 1 [0.8]) {:exposure 1.0})]
        (is (every? #(< (Math/abs (- 1.0 (double %))) 1e-3) (:data out)))))))

(deftest white-balance-in-the-pipeline
  (let [g (uniform 4 4 [128 128 128])
        at (fn [s] (let [p (aget ^ints (display (develop/tone g s)) 5)] [(ch p 16) (ch p 8) (ch p 0)]))]
    (testing "temperature: warm = red above blue, cool = blue above red"
      (let [[r _ b] (at {:temp 0.8})] (is (> r b)))
      (let [[r _ b] (at {:temp -0.8})] (is (< r b))))
    (testing "tint: + = magenta (green below red and blue), - = green"
      (let [[r gr b] (at {:tint 0.8})] (is (< gr (min r b))))
      (let [[r gr b] (at {:tint -0.8})] (is (> gr (min r b)))))))

(deftest tone-effects
  (let [grey (uniform 4 4 [128 128 128])]
    (is (> (ch0 (develop/tone grey {:exposure 1.0}) 5) 128) "+1 stop brightens")
    (is (< (ch0 (develop/tone grey {:exposure -1.0}) 5) 128) "-1 stop darkens")
    (testing "contrast spreads values around mid grey"
      (let [two (from-rgb 2 1 [[80 80 80] [180 180 180]])
            out (display (develop/tone two {:contrast 0.5}))]
        (is (< (ch (aget ^ints out 0) 16) 80))
        (is (> (ch (aget ^ints out 1) 16) 180))))
    (testing "shadows lift and highlights recover"
      (let [two (from-rgb 2 1 [[40 40 40] [220 220 220]])
            base (display two)
            up   (display (develop/tone two {:shadows 1.0}))
            dn   (display (develop/tone two {:highlights -1.0}))]
        (is (> (ch (aget ^ints up 0) 16) 40))
        (is (< (ch (aget ^ints dn 1) 16) 220))
        (is (<= (Math/abs (- (ch (aget ^ints base 1) 16) (ch (aget ^ints up 1) 16))) 3)
            "a shadows move barely touches highlights")))
    (testing "saturation: -1 removes colour, + increases spread"
      (let [col (uniform 2 2 [200 100 40])
            p0  (aget ^ints (display (develop/tone col {:saturation -1.0})) 0)
            p1  (aget ^ints (display (develop/tone col {:saturation 0.5})) 0)]
        (is (<= (- (apply max [(ch p0 16) (ch p0 8) (ch p0 0)]) (apply min [(ch p0 16) (ch p0 8) (ch p0 0)])) 2))
        (is (> (- (ch p1 16) (ch p1 0)) (- 200 40)))))
    (testing "black & white removes colour"
      (let [col (uniform 2 2 [200 100 40])
            p   (aget ^ints (display (develop/tone col {:bw 1.0})) 0)]
        (is (<= (- (apply max [(ch p 16) (ch p 8) (ch p 0)]) (apply min [(ch p 16) (ch p 8) (ch p 0)])) 2))))
    (testing "vignette darkens corners more than the centre"
      (let [big (uniform 40 40 [200 200 200])
            out (display (develop/tone big {:vignette 1.0}))
            at (fn [x y] (ch (aget ^ints out (+ (* y 40) x)) 8))]
        (is (< (at 0 0) (at 20 20)))))
    (testing "fade lifts blacks"
      (is (> (ch0 (develop/tone (uniform 2 2 [0 0 0]) {:fade 1.0}) 0) 20)))
    (testing "grain is deterministic and changes the image"
      (let [a (display (develop/tone grey {:grain 0.8})) b (display (develop/tone grey {:grain 0.8}))]
        (is (= (vec a) (vec b)))
        (is (> (count (distinct (vec a))) 1))))))

(deftest highlight-headroom
  ;; Linear 0.8 pushed +1 stop is 1.6: past white. A plain display clips it, but
  ;; because the engine keeps over-range values, highlights -1 can still pull it down.
  (let [src (grey-scene 2 2 (repeat 4 0.8))]
    (is (= 255 (ch0 (develop/tone src {:exposure 1.0}) 0)))
    (let [v (ch0 (develop/tone src {:exposure 1.0 :highlights -1.0}) 0)]
      (is (< 200 v 250) (str "recovered to " v)))))

(deftest wide-gamut-is-kept-through-editing
  ;; Pure sRGB red pushed to higher saturation leaves sRGB, but stays distinct in
  ;; Display P3; an 8-bit sRGB pipeline would have clipped it before export.
  (let [src (uniform 2 2 [200 60 60])
        out (develop/tone src {:saturation 1.0})
        srgb (aget ^ints (:pixels (scene/->argb out :srgb)) 0)
        p3   (aget ^ints (:pixels (scene/->argb out :display-p3)) 0)]
    (is (= 255 (ch srgb 16)) "clipped in sRGB")
    (is (< (ch p3 16) 255) "inside Display P3")))

;; --- curve -------------------------------------------------------------------

(deftest curve-lut
  (let [id (develop/curve-lut develop/default-curve)]
    (is (= 256 (alength id)))
    (is (every? #(< (Math/abs (- (aget id %) (/ % 255.0))) 1e-9) (range 256)) "identity points -> identity curve"))
  (let [lut (develop/curve-lut [0 0.1 0.5 0.9 1])]
    (is (< (aget lut 64) 0.25) "crushed shadows")
    (is (> (aget lut 192) 0.75) "lifted highlights")
    (is (every? #(<= 0.0 (aget lut %) 1.0) (range 256)))
    (is (= [0.0 1.0] [(aget lut 0) (aget lut 255)]))))

(deftest curve-moves-pixels
  (let [src (uniform 2 2 [128 128 128])]
    (is (< (ch0 (develop/tone src {:curve [0 0.1 0.2 0.5 1]}) 0) 100))
    (is (> (ch0 (develop/tone src {:curve [0 0.4 0.8 0.9 1]}) 0) 150))))

;; --- geometry ----------------------------------------------------------------

(def ^:private src23 (grey-scene 3 2 [1 2 3 4 5 6]))

(deftest geometry-neutral-returns-same
  (is (identical? src23 (develop/geometry src23 {})))
  (is (identical? src23 (develop/geometry src23 {:angle 0.0 :aspect "orig" :flip false}))))

(deftest geometry-flip-and-crop
  (is (= [[3.0 2.0 1.0] [6.0 5.0 4.0]] (grid (develop/geometry src23 {:flip true}))) "mirror")
  (let [wide (grey-scene 4 2 [1 2 3 4 5 6 7 8])
        sq   (develop/geometry wide {:aspect "1:1"})]
    (is (= [2 2] [(:width sq) (:height sq)]))
    (is (= [[2.0 3.0] [6.0 7.0]] (grid sq)) "centred crop keeps the middle columns"))
  (let [out (develop/geometry (grey-scene 160 90 (repeat (* 160 90) 0.5)) {:aspect "4:5"})]
    (is (= [72 90] [(:width out) (:height out)]) "4:5 of a 160x90 frame")))

(deftest geometry-straighten
  (let [uni (uniform 80 60 [90 120 150])
        out (develop/geometry uni {:angle 7.5})]
    (is (= [80 60] [(:width out) (:height out)]))
    (is (every? true? (map #(< (Math/abs (- (double %1) (double %2))) 1e-5) (:data uni) (:data out)))
        "zoom keeps the frame filled: no edge colour leaks in"))
  (let [grad (scene/image 100 100 (float-array (for [_ (range 100) x (range 100) _ (range 3)] (/ x 100.0))))
        out  (develop/geometry grad {:angle 10.0})
        mid  #(lin % (+ (* 50 100) 50) 0)]
    (is (< (Math/abs (- (mid grad) (mid out))) 0.02) "centre pixel stays put")))

;; --- resize ------------------------------------------------------------------

(deftest resize-long-edge
  (let [img (uniform 100 50 [10 20 30])]
    (let [o (develop/resize-long-edge img 20)]
      (is (= [20 10] [(:width o) (:height o)]))
      (is (every? true? (map #(< (Math/abs (- (double %1) (double %2))) 1e-5)
                             (take 3 (:data o)) (take 3 (:data img))))))
    (testing "resampling averages light, not encoded values"
      (let [o (develop/resize-long-edge (grey-scene 2 1 [0.0 1.0]) 1)]
        (is (< (Math/abs (- 0.5 (lin o 0 0))) 1e-6) "halfway between black and white is 50% linear")
        (is (<= 186 (ch0 o 0) 190) "which is about 188 once encoded")))
    (is (identical? img (develop/resize-long-edge img 500)) "never enlarges")
    (is (identical? img (develop/resize-long-edge img 0)) "0 = full resolution")
    (is (identical? img (develop/resize-long-edge img nil)))
    (let [o (develop/resize-long-edge (uniform 1000 400 [1 2 3]) 100)]
      (is (= [100 40] [(:width o) (:height o)])))))
