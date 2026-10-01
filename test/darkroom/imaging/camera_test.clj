(ns darkroom.imaging.camera-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.dcp-fixture :as fx]
            [darkroom.dng :refer [write-dng!]]
            [darkroom.imaging.loader :as loader]
            [darkroom.imaging.raw :as raw]
            [darkroom.imaging.camera :as camera]
            [darkroom.imaging.color :as color]
            [darkroom.imaging.dcp :as dcp]
            [darkroom.imaging.scene :as scene]))

(defn- near? ([a b] (near? a b 1e-6)) ([a b eps] (< (Math/abs (- (double a) (double b))) eps)))

(def ^:private cm fx/srgb-from-xyz-d65) ; a "camera" that sees linear sRGB

(defn- profile [& {:keys [fm hue-sat look curve illum]}]
  (dcp/parse
    (fx/dcp-bytes
      (concat [[50936 2 "Test profile"] [50708 2 "Test camera"]
               [50721 10 cm] [50722 10 cm] [50778 3 [(or (first illum) 17)]] [50779 3 [(or (second illum) 21)]]]
              (when fm [[50964 10 fm] [50965 10 fm]])
              (when hue-sat [[50937 4 (:dims hue-sat)] [50938 11 (:data hue-sat)] [50939 11 (:data hue-sat)]])
              (when look [[50981 4 (:dims look)] [50982 11 (:data look)]])
              (when curve [[50940 11 (vec (mapcat identity curve))]])))))

(deftest parses-a-profile
  (let [p (profile :fm [1 0 0 0 1 0 0 0 1] :hue-sat {:dims [2 2 1] :data (fx/table-floats 2 2 1 (fn [_ _ _] [0.0 1.0 1.0]))}
                   :curve [[0 0] [0.5 0.6] [1 1]])]
    (is (= ["Test profile" "Test camera" 17 21] ((juxt :name :camera :illuminant-1 :illuminant-2) p)))
    (is (every? true? (map near? cm (:color-matrix-1 p))))
    (is (= [2 2 1] (vec (:dims (:hue-sat-1 p)))))
    (is (= [[0.0 0.0] [0.5 0.6000000238418579] [1.0 1.0]] (:tone-curve p)))
    (is (nil? (:look p))))
  (is (thrown? clojure.lang.ExceptionInfo (dcp/parse (byte-array 32))) "not a DCP")
  (is (thrown? clojure.lang.ExceptionInfo (dcp/parse (fx/dcp-bytes [[50936 2 "no matrix"]]))) "no colour matrix"))

(deftest illuminants-and-interpolation-weight
  (is (near? 6504 (camera/xy->cct color/d65-xy) 15) "McCamy on D65")
  (is (near? 5003 (camera/xy->cct color/d50-xy) 15) "McCamy on D50")
  (let [p (profile)
        xyz-a (color/xy->xyz [0.4476 0.4074])            ; CIE illuminant A
        neutral-a (color/mat-vec (vec cm) xyz-a)]        ; what the camera records for it
    (is (near? 0.0 (camera/interpolation-weight p [1 1 1])) "a D65 white sits at illuminant 2")
    (is (near? 1.0 (camera/interpolation-weight p neutral-a) 1e-3) "an A white sits at illuminant 1")
    (let [w (camera/interpolation-weight p (color/mat-vec (vec cm) (color/xy->xyz [0.3457 0.3585])))]
      (is (< 0.0 w 1.0) "D50 is in between"))
    (testing "a single-illuminant profile always weighs its only matrix"
      (is (= 1.0 (camera/interpolation-weight (dcp/parse (fx/dcp-bytes [[50721 10 cm] [50778 3 [21]]])) [1 1 1]))))))

(deftest camera-matrix-maps-neutral-to-d50
  (testing "from a ColorMatrix (adapted from the scene white)"
    (let [m (camera/camera->xyz (profile) [1 1 1])
          xyz (color/mat-vec m [1 1 1])]
      (is (every? true? (map #(near? %1 %2 1e-4) xyz (color/xy->xyz color/d50-xy))))))
  (testing "a warm scene white (camera sees A as [n]) still lands on D50"
    (let [n (color/mat-vec (vec cm) (color/xy->xyz [0.4476 0.4074]))
          n (mapv #(/ % (second n)) n)
          m (camera/camera->xyz (profile) n)]
      (is (every? true? (map #(near? %1 %2 1e-4) (color/mat-vec m [1 1 1]) (color/xy->xyz color/d50-xy))))))
  (testing "a ForwardMatrix is used as given"
    (let [fm [0.9 0.1 0 0 1 0 0 0 0.8]]
      (is (every? true? (map #(near? %1 %2 1e-4) fm (camera/camera->xyz (profile :fm fm) [1 1 1])))))))

(defn- image [w h f]
  (let [a (float-array (* 3 w h))]
    (dotimes [i (* w h)] (let [[r g b] (f i)] (aset a (* 3 i) (float r)) (aset a (+ (* 3 i) 1) (float g)) (aset a (+ (* 3 i) 2) (float b))))
    (scene/image w h a)))

(defn- px [img i] (mapv #(double (aget ^floats (:data img) (+ (* 3 i) %))) (range 3)))

(deftest greys-stay-grey-and-primaries-match-srgb
  (let [src (image 3 1 (fn [i] (case (int i) 0 [0.5 0.5 0.5] 1 [1.0 0.0 0.0] 2 [0.2 0.4 0.6])))
        out (camera/render src (profile) [1 1 1])
        working-from-srgb (color/convert-matrix :srgb :working)]
    (testing "grey in, grey out (a neutral stays neutral through D50 and back to the D65 working space)"
      (is (every? #(near? % 0.5 1e-4) (px out 0))))
    (testing "a camera that sees linear sRGB gives the working-space values of those sRGB colours"
      (doseq [i [1 2]]
        (let [want (color/mat-vec working-from-srgb (px src i))]
          (is (every? true? (map #(near? %1 %2 2e-3) want (px out i))) (str "pixel " i)))))))

(deftest tables-move-colours
  (let [src (image 2 1 (fn [i] (if (zero? i) [0.6 0.3 0.1] [0.3 0.3 0.3])))
        identity-map {:dims [6 3 1] :data (fx/table-floats 6 3 1 (fn [_ _ _] [0.0 1.0 1.0]))}
        direct (camera/render src (profile) [1 1 1])]
    (testing "an identity table changes nothing"
      (let [out (camera/render src (profile :hue-sat identity-map) [1 1 1])]
        (is (every? true? (map #(near? %1 %2 1e-4) (px direct 0) (px out 0))))))
    (testing "saturation scale 0 turns every colour to a grey of the same value; real greys are untouched"
      (let [zero-sat {:dims [6 3 1] :data (fx/table-floats 6 3 1 (fn [_ s _] (if (zero? s) [0.0 1.0 1.0] [0.0 0.0 1.0])))}
            out (camera/render src (profile :hue-sat zero-sat) [1 1 1])
            [r g b] (px out 0)]
        (is (every? #(near? % r 1e-3) [g b]) "grey")
        (is (every? #(near? % 0.3 1e-4) (px out 1)))))
    (testing "a hue shift of 60 degrees rotates red towards yellow"
      (let [shift {:dims [6 3 1] :data (fx/table-floats 6 3 1 (fn [_ s _] (if (zero? s) [0.0 1.0 1.0] [60.0 1.0 1.0])))}
            red (image 1 1 (fn [_] [0.8 0.1 0.1]))
            base (px (camera/render red (profile) [1 1 1]) 0)
            out (px (camera/render red (profile :hue-sat shift) [1 1 1]) 0)]
        (is (> (second out) (second base)) "more green")
        (is (< (nth out 2) (+ 0.05 (nth base 2))) "blue unchanged-ish")))
    (testing "the look table is applied after the hue/sat table"
      (let [darker {:dims [6 3 1] :data (fx/table-floats 6 3 1 (fn [_ _ _] [0.0 1.0 0.5]))}
            out (camera/render src (profile :look darker) [1 1 1])]
        (is (near? (* 0.5 0.3) (first (px out 1)) 1e-3) "value scaled by the look table (grey 0.3 -> 0.15)")))
    (testing "the tone curve is only used when asked for"
      (let [c (profile :curve [[0 0] [0.5 0.25] [1 1]])]
        (is (near? 0.3 (first (px (camera/render src c [1 1 1]) 1)) 1e-4))
        (is (< (first (px (camera/render src c [1 1 1] {:tone-curve? true}) 1)) 0.25) "0.3 on a curve that maps 0.5 -> 0.25")))
    (testing "over-range highlights keep their brightness and hue"
      (let [hot (image 1 1 (fn [_] [4.0 2.0 1.0]))
            base (px (camera/render hot (profile) [1 1 1]) 0)
            out (px (camera/render hot (profile :hue-sat identity-map) [1 1 1]) 0)]
        (is (every? true? (map #(near? %1 %2 1e-3) base out)))))))

(defn- dcp-file ^java.io.File [p-bytes]
  (let [f (java.io.File/createTempFile "profile" ".dcp")]
    (.deleteOnExit f)
    (java.nio.file.Files/write (.toPath f) ^bytes p-bytes (into-array java.nio.file.OpenOption []))
    f))

(defn- centre [sc]
  (let [i (* 3 (+ (* (quot (:height sc) 2) (:width sc)) (quot (:width sc) 2)))]
    (mapv #(double (aget ^floats (:data sc) (+ i %))) (range 3))))

(deftest raw-through-a-profile-agrees-with-libraw-when-the-profile-has-no-look
  ;; the synthetic DNG's camera space is linear sRGB and its ColorMatrix is the sRGB one, so a
  ;; matrix-only profile built from it must reproduce LibRaw's own conversion
  (let [plain (dcp-file (fx/dcp-bytes [[50936 2 "Matrix only"] [50721 10 cm] [50778 3 [21]]]))]
    (doseq [rgb [[40000 10000 5000] [10000 40000 5000] [5000 10000 40000] [30000 30000 30000]]]
      (let [f (write-dng! 64 48 rgb)
            libraw (centre (raw/load-scene f))
            ours   (centre (raw/load-scene-with-profile f (.getPath plain)))]
        (is (every? true? (map #(near? %1 %2 3e-3) libraw ours)) (str rgb ": " libraw " vs " ours))))))

(deftest raw-through-a-profile-with-a-look
  (let [grey-it (dcp-file (fx/dcp-bytes [[50936 2 "Mono"] [50721 10 cm] [50778 3 [21]]
                                         [50937 4 [6 3 1]]
                                         [50938 11 (fx/table-floats 6 3 1 (fn [_ s _] (if (zero? s) [0.0 1.0 1.0] [0.0 0.0 1.0])))]]))
        f (write-dng! 64 48 [40000 10000 5000])
        [r g b] (centre (loader/load-scene (.getPath f) {:camera-profile (.getPath grey-it)}))
        plain (centre (loader/load-scene (.getPath f)))]
    (is (every? #(near? % r 2e-3) [g b]) "the profile desaturated the patch")
    (is (not (near? (first plain) (second plain) 0.05)) "the default decode keeps its colour")
    (testing "non-RAW files ignore the profile"
      (let [png (java.io.File/createTempFile "xyz" ".png")]
        (javax.imageio.ImageIO/write (java.awt.image.BufferedImage. 4 4 java.awt.image.BufferedImage/TYPE_INT_RGB) "png" png)
        (is (= 4 (:width (loader/load-scene (.getPath png) {:camera-profile (.getPath grey-it)}))))))))
