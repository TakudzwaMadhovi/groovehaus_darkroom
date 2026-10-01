(ns darkroom.imaging.color-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.color :as color])
  (:import (java.awt.color ICC_ColorSpace ICC_Profile)))

(defn- near? [expected actual tol]
  (every? true? (map (fn [e a] (<= (Math/abs (- (double e) (double a))) tol)) expected actual)))

(deftest matrix-algebra
  (let [m [2 0 1 1 3 0 0 1 4]]
    (is (near? [1 0 0 0 1 0 0 0 1] (color/mat* m (color/mat-inv m)) 1e-12))
    (is (near? [3 7 6] (color/mat-vec m [1 2 1]) 1e-12) "row-major matrix times column vector"))
  (is (thrown? clojure.lang.ExceptionInfo (color/mat-inv [1 2 3 2 4 6 0 0 1])) "singular"))

(deftest published-rgb-to-xyz-matrices
  (testing "sRGB (IEC 61966-2-1, D65)"
    (is (near? [0.4124564 0.3575761 0.1804375 0.2126729 0.7151522 0.0721750 0.0193339 0.1191920 0.9503041]
               (color/rgb->xyz-matrix :srgb) 1.5e-3)))
  (testing "Adobe RGB (1998), D65"
    (is (near? [0.5767309 0.1855540 0.1881852 0.2973769 0.6273491 0.0752741 0.0270343 0.0706872 0.9911085]
               (color/rgb->xyz-matrix :adobe-rgb) 1.5e-3)))
  (testing "Display P3, D65"
    (is (near? [0.4865709 0.2656677 0.1982173 0.2289746 0.6917385 0.0792869 0.0 0.0451134 1.0439444]
               (color/rgb->xyz-matrix :display-p3) 1.5e-3))))

(deftest white-maps-to-white
  (doseq [sp (keys color/spaces)]
    (is (near? (color/xy->xyz color/d65-xy) (color/mat-vec (color/rgb->xyz-matrix sp) [1 1 1]) 1e-5) (str sp))))

(deftest conversions
  (is (near? [1 0 0 0 1 0 0 0 1] (color/convert-matrix :srgb :srgb) 1e-12))
  (let [there (color/convert-matrix :srgb :working)
        back  (color/convert-matrix :working :srgb)]
    (is (near? [1 0 0 0 1 0 0 0 1] (color/mat* back there) 1e-9) "round trip")
    (is (near? [1 1 1] (color/mat-vec there [1 1 1]) 1e-3) "white stays white (rows sum to 1)")
    (testing "the working space contains sRGB: sRGB primaries stay positive"
      (is (every? #(>= (double %) 0.0) (color/mat-vec there [1 0 0])))
      (is (every? #(>= (double %) -1e-9) (color/mat-vec there [0 0 1])))))
  (testing "sRGB red is inside Display P3 but not the other way round"
    (is (every? #(<= -1e-9 (double %) 1.0) (color/mat-vec (color/convert-matrix :srgb :display-p3) [1 0 0])))
    (is (some neg? (color/mat-vec (color/convert-matrix :display-p3 :srgb) [0 1 0])))))

(deftest luma
  (is (near? [0.2126 0.7152 0.0722] (color/luma-weights :srgb) 1e-3))
  (is (< (Math/abs (- 1.0 (reduce + (color/luma-weights :working)))) 1e-6)))

(deftest transfer-curves
  (doseq [x [0.0 0.001 0.0031308 0.01 0.18 0.5 0.9 1.0 3.0]]
    (is (< (Math/abs (- x (color/srgb-decode (color/srgb-encode x)))) 1e-9)))
  (is (< (Math/abs (- 0.5 (color/srgb-encode 0.21404114))) 1e-6) "mid grey")
  (testing "gamma curve"
    (let [enc (color/trc-encode-fn [:gamma 2.2]) dec (color/trc-decode-fn [:gamma 2.2])]
      (is (< (Math/abs (- 0.5 (enc (dec 0.5)))) 1e-9)))))

(deftest bradford
  (let [m (color/adaptation-matrix color/d65-xy color/d50-xy)]
    (is (near? (color/xy->xyz color/d50-xy) (color/mat-vec m (color/xy->xyz color/d65-xy)) 1e-9)
        "maps the source white exactly onto the destination white")
    (is (near? [1.0478112 0.0228866 -0.0501270] (take 3 m) 1e-3) "published D65->D50 Bradford matrix, first row")))

(deftest white-balance
  (testing "neutral at 0, 0"
    (is (near? [1 0 0 0 1 0 0 0 1] (color/wb-matrix 0 0) 1e-9))
    (is (near? color/d65-xy (color/wb-white 0 0) 1e-9)))
  (testing "warming raises red and lowers blue; cooling does the opposite"
    (let [warm (color/mat-vec (color/wb-matrix 0.6 0) [1 1 1])
          cool (color/mat-vec (color/wb-matrix -0.6 0) [1 1 1])]
      (is (> (warm 0) 1.0)) (is (< (warm 2) 1.0))
      (is (< (cool 0) 1.0)) (is (> (cool 2) 1.0))))
  (testing "the effect grows monotonically with the slider"
    (let [ratio #(let [v (color/mat-vec (color/wb-matrix % 0) [1 1 1])] (/ (v 0) (v 2)))]
      (is (apply < (map ratio [-1 -0.5 0 0.5 1])))))
  (testing "+tint adds magenta (green down), -tint adds green"
    (let [mag (color/mat-vec (color/wb-matrix 0 0.6) [1 1 1])
          grn (color/mat-vec (color/wb-matrix 0 -0.6) [1 1 1])]
      (is (< (mag 1) 1.0)) (is (> (grn 1) 1.0))))
  (testing "slider extremes stay in a believable range of source illuminants"
    (let [[x y] (color/wb-white 1 0)] (is (and (< 0.25 x 0.30) (< 0.26 y 0.31))))
    (let [[x y] (color/wb-white -1 0)] (is (and (< 0.35 x 0.39) (< 0.36 y 0.39))))))

(deftest icc-profiles
  (doseq [sp [:srgb :display-p3 :adobe-rgb]]
    (let [bytes (color/icc-bytes sp)
          p     (ICC_Profile/getInstance ^bytes bytes)
          cs    (ICC_ColorSpace. p)]
      (testing (str sp)
        (is (= java.awt.color.ColorSpace/TYPE_RGB (.getColorSpaceType p)))
        (is (= ICC_Profile/CLASS_DISPLAY (.getProfileClass p)))
        (is (near? [0.9642 1.0 0.8249] (.toCIEXYZ cs (float-array [1 1 1])) 2e-3) "white is the D50 PCS white")))))

(deftest generated-profiles-match-published-colorants
  (testing "Adobe RGB (1998) colorants adapted to D50 (Lindbloom)"
    (let [cs (ICC_ColorSpace. (ICC_Profile/getInstance ^bytes (color/icc-bytes :adobe-rgb)))]
      (is (near? [0.6097559 0.3111242 0.0194811] (.toCIEXYZ cs (float-array [1 0 0])) 1.5e-3))
      (is (near? [0.2052401 0.6256560 0.0608902] (.toCIEXYZ cs (float-array [0 1 0])) 1.5e-3))
      (is (near? [0.1492240 0.0632197 0.7448387] (.toCIEXYZ cs (float-array [0 0 1])) 1.5e-3))
      (testing "tone curve: encoded 0.5 is ~0.5^2.2 of white"
        (is (< (Math/abs (- 0.2176 (aget (.toCIEXYZ cs (float-array [0.5 0.5 0.5])) 1))) 2e-3)))))
  (testing "Display P3 transfer curve is the sRGB curve"
    (let [cs (ICC_ColorSpace. (ICC_Profile/getInstance ^bytes (color/icc-bytes :display-p3)))]
      (is (< (Math/abs (- 0.2140 (aget (.toCIEXYZ cs (float-array [0.5 0.5 0.5])) 1))) 2e-3)))))
