(ns groovehaus.looks.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [groovehaus.looks.blend :as blend]
            [groovehaus.looks.buffer :as buffer]
            [groovehaus.looks.core :as looks])
  (:import [java.awt.image BufferedImage]))

(defn- test-image ^BufferedImage [w h]
  (let [img (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)]
    (dotimes [y h]
      (dotimes [x w]
        (.setRGB img x y (unchecked-int (bit-or 0xFF000000
                                                (bit-shift-left (quot (* 255 x) (dec w)) 16)
                                                (bit-shift-left (quot (* 255 y) (dec h)) 8)
                                                (mod (* 37 (+ x y)) 256))))))
    img))

(defn- pixels [^BufferedImage img]
  (vec (.getRGB img 0 0 (.getWidth img) (.getHeight img) nil 0 (.getWidth img))))

(defn- rgb [^BufferedImage img x y]
  (let [p (.getRGB img x y)]
    [(bit-and 0xff (bit-shift-right p 16)) (bit-and 0xff (bit-shift-right p 8)) (bit-and 0xff p)]))

(deftest buffer-roundtrip-is-lossless
  (let [img (test-image 40 30)]
    (is (= (pixels img) (pixels (buffer/->image (buffer/->buffer img)))))))

(deftest blend-formulas
  (is (= 0.25 (blend/blend-channel :multiply 0.5 0.5)))
  (is (= 0.75 (blend/blend-channel :screen 0.5 0.5)))
  (is (= 0.5 (blend/blend-channel :overlay 0.5 0.5)))
  (is (= 0.2 (blend/blend-channel :darken 0.2 0.9)))
  (is (< (Math/abs (- 0.3 (blend/blend-channel :difference 0.8 0.5))) 1e-9))
  (testing "all modes stay in range over a grid"
    (doseq [m blend/modes b (range 0.0 1.01 0.1) s (range 0.0 1.01 0.1)
            :let [v (blend/blend-channel m b s)]]
      (is (<= -1e-9 v (+ 1.0 1e-9)) (str m " " b " " s)))))

(deftest source-is-never-mutated
  (let [img (test-image 48 32) before (pixels img)]
    (looks/render-stack img [{:look-id :vintage-bw} {:look-id :light-leaks}])
    (is (= before (pixels img)))))

(deftest intensity-zero-is-identity-and-one-normal-is-full-look
  (let [img (test-image 48 32)]
    (is (= (pixels img) (pixels (looks/render-stack img [{:look-id :cross-process :params {:intensity 0}}]))))
    (let [full (looks/render-stack img [{:look-id :cross-process}])
          half (looks/render-stack img [{:look-id :cross-process :params {:intensity 0.5}}])
          [r0] (rgb img 10 10) [r1] (rgb full 10 10) [rh] (rgb half 10 10)]
      (is (not= (pixels img) (pixels full)))
      (is (<= (Math/abs (- rh (/ (+ r0 r1) 2.0))) 1.0) "normal blend at 0.5 is the midpoint"))))

(deftest disabled-layers-are-skipped
  (let [img (test-image 32 32)]
    (is (= (pixels img) (pixels (looks/render-stack img [{:look-id :duotone :enabled false}]))))))

(deftest vintage-bw-is-monochrome-with-dark-corners
  (let [img (test-image 64 48)
        out (looks/render-stack img [{:look-id :vintage-bw :params {:grain-amount 0.3 :fade 0}}])]
    (doseq [[x y] [[5 5] [30 20] [60 40]]
            :let [[r g b] (rgb out x y)]]
      (is (= r g b)))
    (let [flat (BufferedImage. 64 48 BufferedImage/TYPE_INT_ARGB)]
      (doseq [x (range 64) y (range 48)] (.setRGB flat x y (unchecked-int 0xFF808080)))
      (let [o (looks/render-stack flat [{:look-id :vintage-bw
                                         :params {:grain-amount 0 :contrast 0 :fade 0 :vignette-radius 0.3}}])]
        (is (< (first (rgb o 0 0)) (- (first (rgb o 32 24)) 20)) "corner darker than centre")))))

(deftest grain-is-deterministic-and-seeded
  (let [img (test-image 64 48)
        run (fn [seed] (pixels (looks/render-stack img [{:look-id :film-grain :params {:seed seed}}])))]
    (is (= (run 1) (run 1)))
    (is (not= (run 1) (run 2)))))

(deftest duotone-maps-black-and-white-to-endpoints
  (let [img (BufferedImage. 2 1 BufferedImage/TYPE_INT_ARGB)]
    (.setRGB img 0 0 (unchecked-int 0xFF000000))
    (.setRGB img 1 0 (unchecked-int 0xFFFFFFFF))
    (let [out (looks/render-stack img [{:look-id :duotone
                                        :params {:shadow "#ff0000" :highlight [0 0 1] :contrast 0}}])]
      (is (= [255 0 0] (rgb out 0 0)))
      (is (= [0 0 255] (rgb out 1 0))))))

(deftest alpha-is-preserved
  (let [img (BufferedImage. 2 1 BufferedImage/TYPE_INT_ARGB)]
    (.setRGB img 0 0 (unchecked-int 0x80336699))
    (.setRGB img 1 0 (unchecked-int 0x00FFFFFF))
    (let [out (looks/render-stack img [{:look-id :cross-process}])]
      (is (= [0x80 0x00] (mapv #(bit-and 0xff (bit-shift-right (.getRGB out % 0) 24)) [0 1]))))))

(deftest renderer-reuses-unchanged-prefix
  (let [img (test-image 48 32)
        render (looks/renderer img)
        a {:look-id :cross-process}
        b1 {:look-id :film-grain :params {:grain-amount 0.2}}
        b2 {:look-id :film-grain :params {:grain-amount 0.6}}
        calls (atom 0)
        orig @#'looks/apply-normalized]
    (with-redefs [looks/apply-normalized (fn [& args] (swap! calls inc) (apply orig args))]
      (render [a b1])
      (is (= 2 @calls))
      (render [a b2])
      (is (= 3 @calls) "only the edited top layer re-rendered")
      (is (= (pixels (render [a b2])) (pixels (looks/render-stack img [a b2])))))))

(deftest validation
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown look" (looks/render-stack (test-image 4 4) [{:look-id :nope}])))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown params" (looks/render-stack (test-image 4 4) [{:look-id :vintage-bw :params {:bogus 1}}])))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"blend mode" (looks/render-stack (test-image 4 4) [{:look-id :vintage-bw :blend-mode :nope}])))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"expected number" (looks/render-stack (test-image 4 4) [{:look-id :vintage-bw :params {:fade "x"}}])))
  (testing "out-of-range numbers clamp"
    (is (= 1.0 (get-in (looks/default-config :vintage-bw) [:params :intensity])))
    (is (= 1.0 (-> (#'groovehaus.looks.registry/coerce :x {:type :unit} 7) double)))))

(deftest spec-example-config-and-edn-roundtrip
  (let [cfg {:look-id :vintage-bw
             :params {:intensity 0.8 :grain-amount 0.2 :vignette-radius 0.6 :fade 0.1}
             :blend-mode :overlay}
        stack [cfg {:look-id :light-leaks :params {:position :bottom-left :color "#ff8800"}}]]
    (is (= (looks/edn->stack (looks/stack->edn stack))
           (mapv groovehaus.looks.registry/normalize-config stack)))
    (is (instance? BufferedImage (looks/render-stack (test-image 8 8) stack)))))
