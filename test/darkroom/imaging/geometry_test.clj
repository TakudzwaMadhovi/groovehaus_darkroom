(ns darkroom.imaging.geometry-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.geometry :as geometry]
            [darkroom.imaging.scene :as scene]))

(defn- grey-scene [w h vs] (scene/image w h (float-array (mapcat (fn [v] [v v v]) vs))))
(defn- uniform [w h v] (grey-scene w h (repeat (* w h) v)))
(defn- at [sc x y c] (double (aget ^floats (:data sc) (+ (* 3 (+ (* y (:width sc)) x)) c))))
(defn- grid
  "Channel 0 as rows of floats rounded to 4 places."
  [sc]
  (vec (for [y (range (:height sc))]
         (vec (for [x (range (:width sc))] (/ (Math/round (* 1e4 (at sc x y 0))) 1e4))))))
(defn- build [w h f] (scene/image w h (float-array (for [y (range h) x (range w) c (f x y)] c))))
(defn- dims [sc] [(:width sc) (:height sc)])

(def ^:private src23 (grey-scene 3 2 [1 2 3 4 5 6]))

;; --- basics (ported) -------------------------------------------------------------

(deftest geometry-neutral-returns-same
  (is (identical? src23 (geometry/geometry src23 {})))
  (is (identical? src23 (geometry/geometry src23 {:angle 0.0 :aspect "orig" :flip false :flip-v false :rotate 0 :crop nil}))))

(deftest geometry-flip-and-aspect-crop
  (is (= [[3.0 2.0 1.0] [6.0 5.0 4.0]] (grid (geometry/geometry src23 {:flip true}))) "mirror")
  (let [wide (grey-scene 4 2 [1 2 3 4 5 6 7 8])
        sq   (geometry/geometry wide {:aspect "1:1"})]
    (is (= [2 2] (dims sq)))
    (is (= [[2.0 3.0] [6.0 7.0]] (grid sq)) "centred crop keeps the middle columns"))
  (is (= [72 90] (dims (geometry/geometry (uniform 160 90 0.5) {:aspect "4:5"}))) "4:5 of a 160x90 frame"))

(deftest geometry-straighten
  (let [uni (uniform 80 60 0.3)
        out (geometry/geometry uni {:angle 7.5})]
    (is (= [80 60] (dims out)))
    (is (every? true? (map #(< (Math/abs (- (double %1) (double %2))) 1e-5) (:data uni) (:data out)))
        "zoom keeps the frame filled: no edge colour leaks in"))
  (let [grad (build 100 100 (fn [x _] (repeat 3 (/ x 100.0))))
        out  (geometry/geometry grad {:angle 10.0})]
    (is (< (Math/abs (- (at grad 50 50 0) (at out 50 50 0))) 0.02) "centre pixel stays put")))

(deftest resize-long-edge
  (let [img (uniform 100 50 0.4)]
    (let [o (geometry/resize-long-edge img 20)]
      (is (= [20 10] (dims o)))
      (is (every? #(< (Math/abs (- 0.4 (double %))) 1e-5) (:data o))))
    (testing "resampling averages light, not encoded values"
      (let [o (geometry/resize-long-edge (grey-scene 2 1 [0.0 1.0]) 1)]
        (is (< (Math/abs (- 0.5 (at o 0 0 0))) 1e-6) "halfway between black and white is 50% linear")))
    (is (identical? img (geometry/resize-long-edge img 500)) "never enlarges")
    (is (identical? img (geometry/resize-long-edge img 0)) "0 = full resolution")
    (is (identical? img (geometry/resize-long-edge img nil)))
    (is (= [100 40] (dims (geometry/resize-long-edge (uniform 1000 400 0.2) 100))))))

;; --- turns and flips ---------------------------------------------------------------

(deftest quarter-turns
  (testing "clockwise, half turn, counter-clockwise"
    (is (= [[4.0 1.0] [5.0 2.0] [6.0 3.0]] (grid (geometry/geometry src23 {:rotate 1}))))
    (is (= [[6.0 5.0 4.0] [3.0 2.0 1.0]] (grid (geometry/geometry src23 {:rotate 2}))))
    (is (= [[3.0 6.0] [2.0 5.0] [1.0 4.0]] (grid (geometry/geometry src23 {:rotate 3})))))
  (testing "whole turns wrap and the frame swaps for odd turns"
    (is (= (grid (geometry/geometry src23 {:rotate 1})) (grid (geometry/geometry src23 {:rotate 5}))))
    (is (= [2 3] (dims (geometry/geometry src23 {:rotate 1}))))
    (is (identical? src23 (geometry/geometry src23 {:rotate 0})))))

(deftest vertical-flip
  (is (= [[4.0 5.0 6.0] [1.0 2.0 3.0]] (grid (geometry/geometry src23 {:flip-v true}))))
  (testing "both flips = half turn"
    (is (= (grid (geometry/geometry src23 {:rotate 2})) (grid (geometry/geometry src23 {:flip true :flip-v true}))))))

;; --- crop --------------------------------------------------------------------------

(deftest explicit-crop
  (let [img (build 8 4 (fn [x y] (repeat 3 (+ (* 10.0 y) x))))
        out (geometry/geometry img {:crop [0.25 0.5 0.5 0.5]})]
    (is (= [4 2] (dims out)))
    (is (= [[22.0 23.0 24.0 25.0] [32.0 33.0 34.0 35.0]] (grid out)) "columns 2-5, rows 2-3"))
  (testing "an explicit crop wins over the aspect preset (which then only guides the UI)"
    (is (= [4 2] (dims (geometry/geometry (uniform 8 4 0.1) {:crop [0 0 0.5 0.5] :aspect "1:1"})))))
  (testing "a crop is clamped into the frame and keeps at least a pixel"
    (let [[x y w h] (geometry/crop-rect {:crop [0.9 0.9 0.5 0.5]} 100 100)]
      (is (= [90.0 90.0 10.0 10.0] [x y w h])) "the part beyond the frame is cut off")
    (let [[_ _ w h] (geometry/crop-rect {:crop [0.5 0.5 0.0 0.0]} 100 100)]
      (is (= [1.0 1.0] [w h])))))

(deftest crop-is-measured-in-the-turned-frame
  (let [img (build 6 2 (fn [x y] (repeat 3 (+ (* 10.0 y) x))))
        out (geometry/geometry img {:rotate 1 :crop [0 0 1 0.5]})]
    (is (= [2 3] (dims out)) "the frame is 2 wide x 6 tall after a quarter turn; half the height is kept")))

(deftest crop-fractions-and-output-size
  (let [[x y w h] (geometry/crop-fractions {:aspect "4:5"} 160 90)]
    (is (< (Math/abs (- 0.275 x)) 1e-9)) (is (zero? y)) (is (< (Math/abs (- 0.45 w)) 1e-9)) (is (= 1.0 h)))
  (doseq [s [{} {:aspect "16:9"} {:rotate 1} {:crop [0.1 0.2 0.5 0.4]} {:rotate 3 :crop [0 0 0.3 0.9]} {:aspect "3:2" :angle 4.0}]]
    (let [img (uniform 123 77 0.2)]
      (is (= (dims (geometry/geometry img s)) (geometry/output-size s 123 77)) (str s)))))

(deftest cropping-does-not-change-the-zoom
  ;; The fill zoom is worked out for the whole frame, so moving the crop never
  ;; rescales the picture: a cropped render is a window onto the uncropped one.
  (let [img (build 120 80 (fn [x y] (repeat 3 (/ (+ (* 3 x) y) 500.0))))
        base {:angle 6.0 :persp-v 0.3 :distortion 0.2}
        full (geometry/geometry img base)
        win  (geometry/geometry img (assoc base :crop [0.25 0.25 0.5 0.5]))
        ox (long (* 0.25 (:width full))) oy (long (* 0.25 (:height full)))]
    (doseq [[x y] [[0 0] [10 5] [30 20] [55 35]]]
      (is (< (Math/abs (- (at win x y 0) (at full (+ ox x) (+ oy y) 0))) 2e-3) (str [x y])))))

;; --- the frame stays filled ----------------------------------------------------------

;; --- the frame stays filled ----------------------------------------------------------

(deftest zoom-is-only-applied-when-needed
  (testing "turns, flips, crops and barrel correction never need a zoom"
    (doseq [st [{} {:flip true} {:flip-v true} {:rotate 1} {:rotate 2} {:rotate 3}
                {:crop [0.1 0.1 0.5 0.5]} {:aspect "1:1"} {:distortion 1.0}]]
      (is (= 1.0 (geometry/zoom-for st 300 200)) (str st))))
  (testing "straightening, perspective, pincushion correction and CA do"
    (doseq [st [{:angle 3.0} {:angle -8.0} {:persp-v 0.5} {:persp-h -0.5} {:distortion -0.5}
                {:ca-red 1.0} {:ca-blue 1.0}]]
      (is (> (geometry/zoom-for st 300 200) 1.0) (str st))))
  (testing "shrinking a channel's sampling radius stays inside the picture"
    (is (= 1.0 (geometry/zoom-for {:ca-red -1.0 :ca-blue -1.0} 300 200))))
  (testing "more correction needs more zoom"
    (is (< (geometry/zoom-for {:angle 3.0} 300 200) (geometry/zoom-for {:angle 9.0} 300 200)))
    (is (< (geometry/zoom-for {:persp-v 0.3} 300 200) (geometry/zoom-for {:persp-v 0.9} 300 200)))
    (is (< (geometry/zoom-for {:distortion -0.3} 300 200) (geometry/zoom-for {:distortion -1.0} 300 200)))))

(deftest zoom-matches-the-closed-form-for-straightening
  ;; Square frame, angle t: every border pixel centre stays inside at z = cos t + sin t.
  (doseq [deg [2.0 7.5 15.0 30.0]]
    (let [t (Math/toRadians deg)]
      (is (< (Math/abs (- (+ (Math/cos t) (Math/sin t)) (geometry/zoom-for {:angle deg} 801 801))) 1e-4) (str deg)))))

(deftest zoom-is-the-tightest-one-that-fills
  ;; Just below the solved zoom a border pixel centre leaves the source; at it none does.
  (let [params @#'geometry/mapping-params]
    (doseq [st [{:angle 11.0} {:persp-v 0.6 :angle -3.0} {:distortion -0.7} {:ca-red 1.0 :angle 2.0}]]
      (let [iw 400 ih 300 p (params st iw ih) z (geometry/zoom-for st iw ih)
            hw (- (* 0.5 iw) 0.5) hh (- (* 0.5 ih) 0.5)
            pts (concat (for [i (range 0 21)] [(- hw (* i (/ (* 2 hw) 20))) hh])
                        (for [i (range 0 21)] [(- hw (* i (/ (* 2 hw) 20))) (- hh)])
                        (for [i (range 0 21)] [hw (- hh (* i (/ (* 2 hh) 20)))])
                        (for [i (range 0 21)] [(- hw) (- hh (* i (/ (* 2 hh) 20)))]))
            sp @#'geometry/source-point
            outside (fn [zz] (some (fn [[x y]] (some (fn [ca] (let [[sx sy] (sp p zz ca x y)]
                                                                (or (> (Math/abs sx) (+ 1e-6 (- (* 0.5 iw) 0.5)))
                                                                    (> (Math/abs sy) (+ 1e-6 (- (* 0.5 ih) 0.5))))))
                                                     (:ca-extremes p)))
                                   pts))]
        (is (not (outside z)) (str st " fills at " z))
        (is (outside (- z 0.01)) (str st " does not fill just below it"))))))

(deftest straighten-zoom-matches-the-closed-form
  ;; A 100x100 frame rotated 10 degrees needs cos + sin zoom: the sample at the output
  ;; corner (-50, -50) must come from exactly the source edge.
  (let [img (build 200 200 (fn [x _] (repeat 3 (/ x 200.0))))
        out (geometry/geometry img {:angle 10.0})
        th (Math/toRadians 10.0) z (+ (Math/cos th) (Math/sin th))]
    (is (< (Math/abs (- (at out 100 100 0) (at img 100 100 0))) 0.01))
    ;; one output pixel to the right of centre samples z-times closer: horizontal slope scales by 1/z
    (let [slope-out (- (at out 130 100 0) (at out 100 100 0))
          slope-src (- (at img 130 100 0) (at img 100 100 0))]
      (is (< (Math/abs (- (/ slope-out slope-src) (/ (Math/cos th) z))) 0.03)))))

;; --- perspective -----------------------------------------------------------------------

(deftest perspective
  (let [grad (build 200 200 (fn [x _] (repeat 3 (/ x 200.0))))
        spread (fn [sc y] (- (apply max (for [x (range 200)] (at sc x y 0))) (apply min (for [x (range 200)] (at sc x y 0)))))]
    (testing "vertical keystone: + magnifies the top (narrower slice of the source), shrinks the bottom's"
      (let [out (geometry/geometry grad {:persp-v 0.8})]
        (is (< (spread out 5) (spread out 194)))))
    (testing "- does the opposite"
      (let [out (geometry/geometry grad {:persp-v -0.8})]
        (is (> (spread out 5) (spread out 194)))))
    (testing "horizontal keystone leaves a horizontal gradient's rows alike in width"
      (let [out (geometry/geometry grad {:persp-h 0.8})]
        (is (< (Math/abs (- (spread out 5) (spread out 194))) 0.05))))
    (testing "the centre does not move"
      (is (< (Math/abs (- (at grad 100 100 0) (at (geometry/geometry grad {:persp-v 0.8 :persp-h -0.5}) 100 100 0))) 0.02)))))

;; --- lens ---------------------------------------------------------------------------------

(defn- line-x
  "x of the brightest pixel in row y, as its distance from the centre."
  [sc y]
  (let [xs (range (:width sc))
        best (apply max-key #(at sc % y 0) xs)]
    (Math/abs (- best (/ (:width sc) 2.0)))))

(deftest lens-distortion
  ;; a vertical line left of centre; barrel correction (+) pushes it outward at the
  ;; top/bottom rows (more distance from the centre than mid-height), pincushion (-) inward.
  (let [w 400 h 400
        img (build w h (fn [x _] (repeat 3 (if (<= 100 x 102) 1.0 0.0))))
        mid (fn [sc] (line-x sc (quot (:height sc) 2)))
        top (fn [sc] (line-x sc 4))]
    (let [out (geometry/geometry img {:distortion 1.0})]
      (is (> (top out) (+ (mid out) 3.0)) "barrel correction bows the line out at the edge"))
    (let [out (geometry/geometry img {:distortion -1.0})]
      (is (< (top out) (- (mid out) 3.0)) "pincushion correction pulls it in"))
    (let [out (geometry/geometry img {:distortion 0.0 :angle 0.0 :aspect "1:1"})]
      (is (< (Math/abs (- (top out) (mid out))) 1.5) "no distortion: the line is straight"))))

(defn- centroid
  "Brightness-weighted mean x of channel `c` along row `y`."
  [sc y c]
  (let [xs (range (:width sc)) ws (map #(at sc % y c) xs)]
    (/ (reduce + (map * xs ws)) (reduce + ws))))

(deftest chromatic-aberration
  ;; a bright stripe far from the centre, where a 0.5% radial scale moves it ~2 px
  (let [w 1000 h 60
        img (build w h (fn [x _] (repeat 3 (if (<= 948 x 952) 1.0 0.0))))
        plain {:flip false :angle 0.0 :aspect "orig"}]
    (testing "no correction: all channels line up"
      (let [out (geometry/geometry img {:ca-red 0.0 :ca-blue 0.0 :distortion 1e-9})]
        (is (< (Math/abs (- (centroid out 30 0) (centroid out 30 2))) 0.05))))
    (testing "red and blue are scaled radially in opposite senses; green does not move"
      (let [base (geometry/geometry img {:distortion 1e-9})
            out  (geometry/geometry img {:ca-red 1.0 :ca-blue -1.0})
            r (centroid out 30 0) g (centroid out 30 1) b (centroid out 30 2)]
        (is (> (Math/abs (- r g)) 1.5)) (is (> (Math/abs (- b g)) 1.5))
        (is (not= (Math/signum (- r g)) (Math/signum (- b g))))
        (testing "green is the reference: it only moves with the fill zoom (needed for red's outward scale)"
          (let [z (geometry/zoom-for {:ca-red 1.0 :ca-blue -1.0} w h)]
            (is (> z 1.0))
            (is (< (Math/abs (- (- g 499.5) (* z (- 950.0 499.5)))) 0.25))))))))

(deftest geometry-settings-are-tracked
  (doseq [k [:flip-v :rotate :crop :persp-v :persp-h :distortion :ca-red :ca-blue]]
    (is (some #{k} geometry/geometry-keys) (str k))
    (is (not (geometry/geometry-neutral? {k (case k :flip-v true :rotate 1 :crop [0 0 0.5 0.5] 0.4)})) (str k))))
