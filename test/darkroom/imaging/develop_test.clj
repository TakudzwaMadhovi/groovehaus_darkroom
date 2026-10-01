(ns darkroom.imaging.develop-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.color :as color]
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

;; --- new tone controls --------------------------------------------------------

(defn- rgb-of
  "[r g b] 8-bit sRGB of the first pixel after tone with `settings`."
  [rgb settings]
  (let [p (aget ^ints (display (develop/tone (uniform 2 2 rgb) settings)) 0)]
    [(ch p 16) (ch p 8) (ch p 0)]))

(defn- chroma [[r g b]] (- (max r g b) (min r g b)))
(defn- luma [[r g b]] (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b)))

(deftest whites-and-blacks
  (testing "whites move the highlights and leave the shadows alone"
    (is (> (first (rgb-of [230 230 230] {:whites 1.0})) 235))
    (is (< (first (rgb-of [230 230 230] {:whites -1.0})) 225))
    (is (<= (Math/abs (- 30 (first (rgb-of [30 30 30] {:whites 1.0})))) 2)))
  (testing "blacks lift (+) or crush (-) the shadows and leave the highlights alone"
    (is (> (first (rgb-of [20 20 20] {:blacks 1.0})) 40))
    (is (< (first (rgb-of [40 40 40] {:blacks -1.0})) 25))
    (is (<= (Math/abs (- 220 (first (rgb-of [220 220 220] {:blacks 1.0})))) 2)))
  (testing "both are monotonic: a darker input never comes out lighter"
    (doseq [s [{:whites 1.0 :blacks 1.0} {:whites -1.0 :blacks -1.0} {:whites 1.0 :blacks -1.0} {:whites -1.0 :blacks 1.0}]]
      (let [outs (map #(first (rgb-of [% % %] s)) (range 0 256 5))]
        (is (apply <= outs) (str s))))))

(deftest vibrance
  (let [muted [150 120 110] vivid [250 40 40]
        gain  (fn [c s] (- (chroma (rgb-of c s)) (chroma (rgb-of c {}))))]
    (testing "+vibrance lifts muted colours more than saturated ones"
      (is (> (gain muted {:vibrance 1.0}) 8))
      (is (> (gain muted {:vibrance 1.0}) (* 2 (max 1 (gain vivid {:vibrance 1.0}))))))
    (testing "-vibrance reduces colour"
      (is (< (gain muted {:vibrance -1.0}) -8)))
    (testing "skin-tone oranges are protected relative to other muted hues"
      (let [skin [200 150 120] blue [110 130 170]]
        (is (< (gain skin {:vibrance 1.0}) (gain blue {:vibrance 1.0})))))
    (testing "greys are unaffected"
      (is (= (rgb-of [128 128 128] {}) (rgb-of [128 128 128] {:vibrance 1.0}))))))

(defn- hsl-with
  "An HSL mixer value with `[h s l]` set on the band named `band`."
  [band hsl3]
  (assoc develop/default-hsl (.indexOf ^java.util.List (mapv first develop/hsl-bands) band) hsl3))

(deftest hsl-mixer
  (let [red [200 60 60] green [60 200 60] blue [60 60 200]]
    (testing "default mixer is a no-op"
      (is (= (rgb-of red {}) (rgb-of red {:hsl develop/default-hsl}))))
    (testing "red hue +1 shifts red toward orange (more green, same blue); green is untouched"
      (let [[r g _] (rgb-of red {:hsl (hsl-with "RED" [1.0 0.0 0.0])})]
        (is (> g 90)) (is (> r g)))
      (is (= (rgb-of green {}) (rgb-of green {:hsl (hsl-with "RED" [1.0 1.0 1.0])}))))
    (testing "red saturation -1 drains most of the red (the pixel sits between bands, so not all)"
      (is (< (chroma (rgb-of red {:hsl (hsl-with "RED" [0.0 -1.0 0.0])})) (* 0.6 (chroma (rgb-of red {}))))))
    (testing "all bands at -1 saturation = a full desaturation"
      (doseq [c [red green blue [200 120 40] [150 60 170]]]
        (is (< (chroma (rgb-of c {:hsl (vec (repeat 8 [0.0 -1.0 0.0]))})) 6) (str c))))
    (testing "blue luminance +1 brightens blue, -1 darkens it"
      (is (> (luma (rgb-of blue {:hsl (hsl-with "BLUE" [0.0 0.0 1.0])})) (+ 5 (luma (rgb-of blue {})))))
      (is (< (luma (rgb-of blue {:hsl (hsl-with "BLUE" [0.0 0.0 -1.0])})) (- (luma (rgb-of blue {})) 5))))
    (testing "greys are never touched, whatever the mixer says"
      (let [wild (vec (repeat 8 [1.0 1.0 1.0]))]
        (is (= (rgb-of [128 128 128] {}) (rgb-of [128 128 128] {:hsl wild})))))
    (testing "bands blend: yellow hue +1 moves an orange pixel (between ORANGE and YELLOW) smoothly"
      (let [orange [230 140 40]
            a (rgb-of orange {:hsl (hsl-with "YELLOW" [1.0 0.0 0.0])})]
        (is (not= a (rgb-of orange {})))))))

(deftest hsl-lut-covers-every-hue-and-wraps
  (let [lut (#'develop/hsl-lut (hsl-with "RED" [1.0 0.5 -0.5]))]
    (is (= 1080 (alength lut)))
    (is (< (Math/abs (- 30.0 (aget lut 0))) 1e-9) "full red band at hue 0")
    (is (< (Math/abs (- 15.0 (aget lut (* 3 15)))) 1e-9) "halfway to ORANGE")
    (is (< (Math/abs (aget lut (* 3 30))) 1e-9) "none at ORANGE")
    (is (> (aget lut (* 3 359)) 25.0) "wraps from MAGENTA back to RED")))

(deftest split-toning
  (let [dark [40 40 40] bright [220 220 220]
        blue-ish (fn [[r _ b]] (- b r))]
    (testing "shadow toning tints shadows toward its hue and leaves highlights"
      (let [s {:split-sh-hue 220.0 :split-sh-sat 1.0}]
        (is (> (blue-ish (rgb-of dark s)) 8))
        (is (<= (Math/abs (blue-ish (rgb-of bright s))) 2))))
    (testing "highlight toning (orange) warms highlights and leaves shadows"
      (let [s {:split-hl-hue 40.0 :split-hl-sat 1.0}]
        (is (< (blue-ish (rgb-of bright s)) -8))
        (is (<= (Math/abs (blue-ish (rgb-of dark s))) 2))))
    (testing "toning shifts colour, not brightness (luma of the encoded working-space values)"
      (let [s {:split-sh-hue 220.0 :split-sh-sat 1.0 :split-hl-hue 40.0 :split-hl-sat 1.0}
            lw (color/luma-weights :working)
            enc-luma (fn [rgb settings]
                       (let [o (develop/tone (uniform 2 2 rgb) settings)]
                         (reduce + (map-indexed (fn [c w] (* w (scene/srgb-encode-extended (lin o 0 c)))) lw))))]
        (doseq [c [dark bright [128 128 128] [200 120 60]]]
          (is (< (Math/abs (- (enc-luma c s) (enc-luma c {}))) 0.01) (str c)))))
    (testing "balance moves the shadow/highlight split"
      (let [mid [128 128 128]
            s {:split-sh-hue 220.0 :split-sh-sat 1.0}]
        (is (> (blue-ish (rgb-of mid (assoc s :split-balance 1.0)))
               (blue-ish (rgb-of mid (assoc s :split-balance -1.0)))))))
    (testing "saturation 0 means off, whatever the hue"
      (is (= (rgb-of dark {}) (rgb-of dark {:split-sh-hue 10.0 :split-hl-hue 200.0}))))))

(deftest per-channel-curves
  ;; Channel curves act on the working space's own channels (as Lightroom's do on
  ;; ProPhoto), so check the working-space data, not the sRGB view of it.
  (let [src (grey-scene 2 2 (repeat 4 0.2159)) low [0.0 0.1 0.25 0.5 1.0]
        chans (fn [s] (let [o (develop/tone src s)] [(lin o 0 0) (lin o 0 1) (lin o 0 2)]))
        [r0 g0 b0] (chans {})]
    (let [[r g b] (chans {:curve-r low})]
      (is (< r (* 0.7 r0)) "red goes down") (is (< (Math/abs (- g g0)) 1e-3)) (is (< (Math/abs (- b b0)) 1e-3)))
    (let [[r g b] (chans {:curve-b low})]
      (is (< b (* 0.7 b0))) (is (< (Math/abs (- r r0)) 1e-3)) (is (< (Math/abs (- g g0)) 1e-3)))
    (testing "the master curve applies to all channels, then the channel curve on top"
      (let [[r g b] (chans {:curve [0 0.4 0.8 0.9 1] :curve-r low})]
        (is (> g r)) (is (< (Math/abs (- g b)) 1e-3))))
    (is (not (develop/tone-neutral? {:curve-g low})))))

(deftest every-tone-key-is-tracked
  (doseq [k [:whites :blacks :vibrance :hsl :split-sh-sat :split-hl-sat :split-balance :curve-r :curve-g :curve-b]]
    (is (some #{k} develop/tone-keys) (str k))
    (is (contains? develop/defaults k) (str k))))

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
