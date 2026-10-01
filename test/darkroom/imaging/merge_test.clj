(ns darkroom.imaging.merge-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.merge :as merge]
            [darkroom.imaging.scene :as scene]))

(defn- value-noise
  "Smooth deterministic noise in 0-1 with features about `cell` pixels wide."
  [w h cell seed]
  (let [r (java.util.Random. seed) gw (+ 2 (quot w cell)) gh (+ 2 (quot h cell))
        g (vec (repeatedly (* gw gh) #(.nextDouble r)))
        at (fn [x y] (g (+ (* y gw) x)))
        a (double-array (* w h))]
    (dotimes [y h]
      (dotimes [x w]
        (let [fx (/ (double x) cell) fy (/ (double y) cell) ix (int fx) iy (int fy) tx (- fx ix) ty (- fy iy)
              sx (* tx tx (- 3 (* 2 tx))) sy (* ty ty (- 3 (* 2 ty)))]
          (aset a (+ (* y w) x)
                (+ (* (- 1 sy) (+ (* (- 1 sx) (at ix iy)) (* sx (at (inc ix) iy))))
                   (* sy (+ (* (- 1 sx) (at ix (inc iy))) (* sx (at (inc ix) (inc iy))))))))))
    a))

(defn- texture
  "A deterministic, natural-looking float RGB field, brightness about 0.01 .. 6 (dark on the left, bright on the right)."
  [w h]
  (let [n1 (value-noise w h 4 1) n2 (value-noise w h 16 2) n3 (value-noise w h 64 3)
        a (float-array (* 3 w h))]
    (dotimes [i (* w h)]
      (let [x (rem i w)
            detail (+ 0.5 (* 0.2 (aget n1 i)) (* 0.3 (aget n2 i)) (* 0.5 (aget n3 i)))
            v (* (+ 0.01 (* 5.0 (Math/pow (/ (double x) w) 2.0))) detail)]
        (aset a (* 3 i) (float v))
        (aset a (+ 1 (* 3 i)) (float (* 0.8 v)))
        (aset a (+ 2 (* 3 i)) (float (* 0.6 v)))))
    (scene/image w h a)))

(defn- expose [{:keys [width height data]} k]
  (let [^floats d data out (float-array (alength d))]
    (dotimes [i (alength d)] (aset out i (float (Math/min 1.0 (* k (double (aget d i)))))))
    (scene/image width height out)))

(defn- near? [a b tol] (< (Math/abs (- (double a) (double b))) tol))

(deftest recovers-radiance-from-a-bracket
  (let [truth (texture 160 120)
        imgs [(expose truth 0.125) (expose truth 0.5) (expose truth 2.0)]   ; reference = the middle one
        out (merge/merge-hdr imgs [0.25 1.0 4.0])
        ^floats t (:data truth) ^floats o (:data out)
        ;; everywhere at least one frame is neither clipped nor black the result must be the truth (in reference units: x 0.5)
        errs (for [i (range (alength t)) :let [v (* 0.5 (double (aget t i)))] :when (< 0.002 v 3.0)]
               (/ (Math/abs (- (double (aget o i)) v)) v))]
    (is (< (apply max errs) 0.02) "relative error under 2%")
    (testing "highlights the reference clipped are recovered above 1"
      (is (> (apply max (map double o)) 1.5)))
    (testing "the dark end is as good as the brightest frame lets it be"
      (let [i (* 3 (+ (* 10 160) 2))]
        (is (near? (aget o i) (* 0.5 (aget t i)) (* 0.03 (* 0.5 (aget t i)))))))))

(deftest all-frames-clipped-falls-back-to-the-shortest
  (let [a (scene/image 1 1 (float-array [1.0 1.0 1.0]))
        b (scene/image 1 1 (float-array [1.0 1.0 1.0]))
        out (merge/merge-hdr [a b] [1.0 4.0])]
    (is (every? #(near? % 1.0 1e-6) (:data out)))))

(deftest exposure-from-camera-settings
  (is (near? 0.04 (merge/exposure-value {:exposure-time [1 250] :f-number [10 1] :iso 1000}) 1e-9) "t * iso / f^2")
  (is (nil? (merge/exposure-value {:exposure-time [1 250] :f-number [10 1]})) "no ISO")
  (is (near? (* 2 (merge/exposure-value {:exposure-time [1 250] :f-number [28 10] :iso 100}))
             (merge/exposure-value {:exposure-time [1 125] :f-number [28 10] :iso 100}) 1e-9)))

(deftest estimates-factors-from-the-pictures
  (let [truth (texture 160 120)
        imgs [(expose truth 0.4) (expose truth 0.1) (expose truth 0.2)]
        f (merge/estimate-factors imgs 0)]
    (is (= 1.0 (f 0)))
    (is (near? 0.25 (f 1) 0.01))
    (is (near? 0.5 (f 2) 0.01))
    (testing "a wide bracket (four stops) is related through its middle frame"
      (let [wide [(expose truth 0.06) (expose truth 0.25) (expose truth 1.0)]
            f (merge/estimate-factors wide 1)]
        (is (near? 0.25 (f 0) 0.02)) (is (near? 4.0 (f 2) 0.1)))))
  (testing "frames that share no well-exposed pixels cannot be related"
    (is (thrown? clojure.lang.ExceptionInfo
                 (merge/estimate-factors [(scene/image 2 2 (float-array 12)) (scene/image 2 2 (float-array 12))] 0)))))

(deftest alignment-finds-the-shift
  (let [truth (texture 320 240)
        a (expose truth 1.0)
        shifted (merge/shift-image (expose truth 0.5) 3 -2)               ; a hand-held frame
        {:keys [images shifts]} (merge/align [a shifted] 0)]
    (is (= [3 -2] (second shifts)) "the frame was found displaced by (3, -2)")
    (testing "after alignment the interior matches the reference scene (at half exposure)"
      (let [^floats x (:data (images 1)) ^floats y (:data a)
            i (fn [px py] (* 3 (+ (* py 320) px)))]
        (is (every? (fn [[px py]] (near? (aget x (i px py)) (* 0.5 (aget y (i px py))) 0.02)) [[30 40] [60 100] [90 60] [100 200]]))))))

(deftest bracket-end-to-end
  (let [truth (texture 320 240)
        frames [{:scene (expose truth 2.0) :tags {:exposure-time [1 25] :f-number [8 1] :iso 100}}
                {:scene (merge/shift-image (expose truth 0.125) 2 1) :tags {:exposure-time [1 400] :f-number [8 1] :iso 100}}
                {:scene (expose truth 0.5) :tags {:exposure-time [1 100] :f-number [8 1] :iso 100}}]
        r (merge/merge-bracket frames {})]
    (is (= :exif (:source r)))
    (is (every? true? (map #(near? %1 %2 1e-9) [4.0 0.25 1.0] (:factors r))) "relative to the middle exposure (1/100 s)")
    (is (= [2 1] (second (:shifts r))) "the hand-held frame was found")
    (let [no-exif (merge/merge-bracket (mapv #(assoc % :tags {}) frames) {})]
      (is (= :estimated (:source no-exif)))
      (is (near? 0.25 (second (:factors no-exif)) 0.02)))
    (is (thrown? clojure.lang.ExceptionInfo (merge/merge-bracket [(first frames)] {})))
    (is (thrown? clojure.lang.ExceptionInfo (merge/merge-bracket [(first frames) {:scene (texture 10 10) :tags {}}] {})))))
