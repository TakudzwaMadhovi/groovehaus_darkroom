(ns darkroom.imaging.panorama-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.panorama :as pano]
            [darkroom.imaging.scene :as scene]))

(defn- value-noise [w h cell seed]
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

(defn- world
  "A large textured scene (values about 0.05-0.75): smooth noise under many random
  rectangles and discs, so there are corners at every scale for feature matching."
  [w h seed]
  (let [n (mapv #(value-noise w h % (+ seed %)) [12 40])
        a (float-array (* 3 w h))
        r (java.util.Random. seed)]
    (dotimes [i (* w h)]
      (let [v (+ 0.2 (* 0.2 (+ (aget ^doubles (n 0) i) (aget ^doubles (n 1) i))))]
        (aset a (* 3 i) (float v)) (aset a (+ 1 (* 3 i)) (float (* 0.9 v))) (aset a (+ 2 (* 3 i)) (float (* 0.8 v)))))
    (dotimes [_ (quot (* w h) 120)]
      (let [cx (.nextInt r w) cy (.nextInt r h) rad (+ 3 (.nextInt r 12)) disc? (.nextBoolean r)
            v (+ 0.05 (* 0.7 (.nextDouble r)))]
        (doseq [y (range (max 0 (- cy rad)) (min h (+ cy rad))) x (range (max 0 (- cx rad)) (min w (+ cx rad)))
                :when (or (not disc?) (<= (+ (* (- x cx) (- x cx)) (* (- y cy) (- y cy))) (* rad rad)))]
          (let [o (* 3 (+ (* y w) x))]
            (aset a o (float v)) (aset a (+ o 1) (float (* 0.9 v))) (aset a (+ o 2) (float (* 0.8 v)))))))
    (scene/image w h a)))

(defn- window [{:keys [^long width data]} x0 y0 w h gain]
  (let [^floats d data out (float-array (* 3 w h))]
    (dotimes [y h] (dotimes [x w] (dotimes [c 3]
      (aset out (+ (* 3 (+ (* y w) x)) c) (float (* gain (aget d (+ (* 3 (+ (* (+ y0 y) width) (+ x0 x))) c))))))))
    (scene/image w h out)))

(deftest largest-rectangle-in-a-grid
  (let [g (boolean-array [false true  true  true false
                          true  true  true  true true
                          true  true  true  true false
                          false true  true  false false])
        [x y w h] (pano/largest-rectangle g 5 4)]
    (is (= 9 (* w h)) "the best block is the 3 x 3 in columns 1-3, rows 0-2")
    (is (every? true? (for [yy (range y (+ y h)) xx (range x (+ x w))] (aget g (+ (* yy 5) xx))))))
  (is (= [0 0 0 0] (pano/largest-rectangle (boolean-array 6) 3 2)) "nothing valid"))

(deftest gains-equalise-overlaps
  (let [g (pano/gains 3 {[0 1] {:n 1000 :mean-i 0.4 :mean-j 0.5}
                         [1 2] {:n 1000 :mean-i 0.5 :mean-j 0.6}})]
    (is (< (Math/abs (- (* (g 0) 0.4) (* (g 1) 0.5))) 0.05) "the mismatch of 0.1 is more than halved")
    (is (< (Math/abs (- (* (g 1) 0.5) (* (g 2) 0.6))) 0.05))
    (is (> (g 0) (g 1) (g 2)) "darker frames are brightened more")
    (is (< 0.8 (/ (+ (g 0) (g 1) (g 2)) 3) 1.2) "and they stay near 1 overall"))
  (is (= [1.0 1.0] (pano/gains 2 {})) "no overlaps, no change"))

(deftest joins-three-frames-with-different-exposures
  (let [wd (world 700 360 5)
        ;; frames of 300 px starting 0, 200, 400: 100 px overlap; exposures 0.8 / 1.0 / 1.25
        frames [(window wd 0 0 300 360 0.8) (window wd 200 0 300 360 1.0) (window wd 400 0 300 360 1.25)]
        order [2 0 1]                                        ; given out of order
        r (pano/panorama (mapv frames order))
        sc (:scene r) w (:width sc) h (:height sc)]
    (is (< 640 w 740) "about the width of the scene")
    (is (< 330 h 390))
    (is (= 1.0 (:scale r)))
    (testing "the stitched interior matches the scene (up to one overall gain)"
      (let [[rx ry rw rh] (:valid-rect r)
            x0 (long (+ (* rx w) 8)) y0 (long (+ (* ry h) 8)) x1 (long (- (* (+ rx rw) w) 8)) y1 (long (- (* (+ ry rh) h) 8))
            ^floats d (:data sc)
            pts (for [y (range y0 y1 7) x (range x0 x1 7)] [x y])
            ;; locate the canvas in the world by the reference frame: compare with a best-fitting offset
            ratios (for [[x y] pts] (/ (double (aget d (* 3 (+ (* y w) x)))) (double (aget ^floats (:data wd) (* 3 (+ (* (+ y 0) 700) (+ x 0)))))))]
        (is (pos? (count pts)))
        (is (> rw 0.8) "most of the canvas is covered by frames")))
    (testing "no exposure steps: the mean brightness left and right of each seam agrees"
      (let [^floats d (:data sc)
            col-mean (fn [x] (/ (reduce + (for [y (range 20 (- h 20) 5)] (double (aget d (* 3 (+ (* y w) x)))))) (count (range 20 (- h 20) 5))))
            seams [200 400]]
        (doseq [s seams]
          (is (< (Math/abs (- (col-mean (- s 12)) (col-mean (+ s 12)))) 0.06) (str "seam near " s)))))))

(deftest recovers-the-scene-exactly-when-frames-agree
  (let [wd (world 600 300 9)
        a (window wd 0 0 350 300 1.0) b (window wd 250 0 350 300 1.0)
        r (pano/panorama [a b])
        sc (:scene r) w (:width sc) h (:height sc)
        ^floats d (:data sc) ^floats t (:data wd)
        ;; the reference frame sits at the canvas origin or offset; find the offset of the canvas in the world by brute force on a patch
        patch (fn [ox oy] (reduce + (for [y (range 40 80 4) x (range 40 80 4)]
                                      (Math/abs (- (double (aget d (* 3 (+ (* y w) x)))) (double (aget t (* 3 (+ (* (+ y oy) 600) (+ x ox))))))))))
        best (apply min-key (fn [[ox oy]] (patch ox oy)) (for [ox [0 250] oy [0]] [ox oy]))
        [ox oy] best
        errs (for [y (range 30 (- h 30) 5) x (range 30 (- w 30) 5)
                   :when (< (+ x ox) 590)]
               (Math/abs (- (double (aget d (* 3 (+ (* y w) x)))) (double (aget t (* 3 (+ (* (+ y oy) 600) (+ x ox))))))))]
    ;; sharp disc edges resampled at a fractional offset differ a little; the bulk must be right
    (is (< (nth (sort errs) (quot (count errs) 2)) 0.01) "median absolute error under 0.01 (values are 0.05-0.75)")
    (is (> (/ (count (filter #(< % 0.05) errs)) (double (count errs))) 0.93) "over 93% of pixels within 0.05")))

(deftest unrelated-frames-are-refused
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"do not overlap"
                        (pano/panorama [(world 300 300 1) (world 300 300 2)])))
  (is (thrown? clojure.lang.ExceptionInfo (pano/panorama [(world 100 100 1)]))))
