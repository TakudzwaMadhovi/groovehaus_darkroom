(ns groovehaus.vector.trace-test
  (:require [clojure.test :refer [deftest is testing]]
            [groovehaus.vector.path :as p]
            [groovehaus.vector.trace :as t])
  (:import [java.awt Color RenderingHints]
           [java.awt.image BufferedImage]))

(defn- mask-image
  "Opaque white-on-black image; `draw` gets a Graphics2D (antialiasing off) with white selected."
  ^BufferedImage [w h draw]
  (let [img (BufferedImage. w h BufferedImage/TYPE_INT_RGB)
        g (.createGraphics img)]
    (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_OFF)
    (.setColor g Color/WHITE)
    (draw g)
    (.dispose g)
    img))

(defn- px! [^BufferedImage img coords]
  (doseq [[x y] coords] (.setRGB img x y 0xFFFFFF)))

(defn- near? [a b tol] (< (Math/abs (- (double a) (double b))) tol))

(deftest solid-square
  (let [img (mask-image 20 20 #(.fillRect % 5 5 8 8))
        [c :as cs] (t/trace-contours img {})]
    (is (= 1 (count cs)))
    (is (false? (:hole? c)))
    (is (= 4 (count (:points c))) "collinear boundary pixels collapse to 4 corners")
    (is (near? 49.0 (:area c) 1e-9) "7x7 pixel-centre polygon (8px block, centres inset by 1)")
    (is (= #{[5.5 5.5] [12.5 5.5] [12.5 12.5] [5.5 12.5]} (set (:points c))))))

(deftest hole-is-a-separate-flagged-contour
  (let [img (mask-image 20 20 #(do (.fillRect % 2 2 14 14) (.setColor % Color/BLACK) (.fillRect % 6 6 4 4)))
        cs (t/trace-contours img {:min-area 0})]
    (is (= [false true] (mapv :hole? (sort-by :hole? cs))))
    (is (> (:area (first (filter (complement :hole?) cs))) (:area (first (filter :hole? cs)))))))

(deftest separate-blobs-and-nesting
  (let [img (mask-image 40 20 #(do (.fillRect % 1 1 8 8) (.fillRect % 20 5 6 6)
                                   ;; island inside a hole
                                   (.fillRect % 30 1 9 18) (.setColor % Color/BLACK) (.fillRect % 32 3 5 14)
                                   (.setColor % Color/WHITE) (.fillRect % 33 8 3 3)))
        cs (t/trace-contours img {:min-area 0})]
    (is (= 2 (count (remove :hole? (filter #(< (first (first (:points %))) 28) cs)))) "two left blobs")
    (is (= [false false false true false] (mapv :hole? (sort-by #(first (first (:points %))) cs)))
        "2 blobs, then ring outer + ring hole + island inside the hole")
    (is (= 5 (count cs)))))

(deftest eight-connectivity
  (let [img (BufferedImage. 8 8 BufferedImage/TYPE_INT_RGB)]
    (px! img [[1 1] [2 2] [3 3] [4 4]])
    (testing "a diagonal chain is one 8-connected region (degenerate, zero area)"
      (is (= 1 (count (t/trace-mask (t/mask img {})))))
      (is (empty? (t/trace-contours img {})) "dropped by default :min-area"))
    (is (= 1 (count (t/trace-contours img {:min-area 0 :epsilon 0}))) "kept with min-area 0")))

(deftest isolated-pixel-and-empty
  (let [img (BufferedImage. 6 6 BufferedImage/TYPE_INT_RGB)]
    (is (= "" (t/trace->path-d img)) "empty mask -> empty path")
    (px! img [[3 3]])
    (is (= [[[3 3]]] (mapv :points (t/trace-mask (t/mask img {})))) "single pixel is a one-point contour")
    (is (= "" (t/trace->path-d img)))))

(deftest raw-boundary-is-complete-and-on-the-boundary
  (let [img (mask-image 30 30 #(.fillOval % 4 4 20 20))
        m (t/mask img {})
        [outer] (t/trace-mask m)
        fg? (fn [x y] (and (< -1 x 30) (< -1 y 30) (== 1 (aget ^bytes (:data m) (+ (* y 30) x)))))
        boundary? (fn [[x y]] (and (fg? x y) (some (fn [[dx dy]] (not (fg? (+ x dx) (+ y dy)))) [[1 0] [-1 0] [0 1] [0 -1]])))]
    (is (every? boundary? (:points outer)) "every traced point is a foreground pixel touching background")
    (testing "every boundary pixel is visited"
      (let [all (for [y (range 30) x (range 30) :when (boundary? [x y])] [x y])]
        (is (= (set all) (set (:points outer))))))))

(deftest circle-area-and-simplification
  (let [img (mask-image 100 100 #(.fillOval % 10 10 80 80))
        fine (first (t/trace-contours img {:epsilon 0}))
        coarse (first (t/trace-contours img {:epsilon 1.5}))]
    (is (near? (* Math/PI 39.5 39.5) (:area fine) (* 0.04 Math/PI 39.5 39.5)) "area ~ pi r^2")
    (is (< (count (:points coarse)) (/ (count (:points fine)) 2)) "RDP reduces points substantially")
    (is (near? (:area fine) (:area coarse) (* 0.05 (:area fine))) "...without changing the shape much")))

(deftest threshold-invert-and-alpha-masks
  (let [img (BufferedImage. 10 10 BufferedImage/TYPE_INT_RGB)]
    (doseq [y (range 2 8) x (range 2 8)] (.setRGB img x y 0x707070))
    (is (empty? (t/trace-contours img {})) "grey 112 is below the default threshold 128")
    (is (= 1 (count (t/trace-contours img {:threshold 100}))))
    (testing "invert traces the background (outer frame + hole)"
      (is (= 2 (count (t/trace-contours img {:threshold 100 :invert? true :min-area 0}))))))
  (testing "alpha cutout: any transparency switches to alpha mode, colour is ignored"
    (let [img (BufferedImage. 12 12 BufferedImage/TYPE_INT_ARGB)]
      (doseq [y (range 3 9) x (range 3 9)] (.setRGB img x y (unchecked-int 0xFF000000))) ; opaque BLACK
      (let [cs (t/trace-contours img {})]
        (is (= 1 (count cs)))
        (is (near? 25.0 (:area (first cs)) 1e-9))))))

(deftest path-d-output-is-valid-svg-path
  (let [img (mask-image 20 20 #(.fillRect % 5 5 8 8))
        d (t/trace->path-d img)
        segs (p/parse-path d)]
    (is (= "M5.5 5.5 L5.5 12.5 L12.5 12.5 L12.5 5.5 Z" d) "starts at the first boundary pixel in raster order")
    (is (= [:move :line :line :line :close] (mapv :op segs))))
  (testing "holes become extra subpaths in one d string"
    (let [img (mask-image 20 20 #(do (.fillRect % 2 2 14 14) (.setColor % Color/BLACK) (.fillRect % 6 6 4 4)))
          segs (p/parse-path (t/trace->path-d img {:min-area 0}))]
      (is (= 2 (count (filter #(= :move (:op %)) segs))))
      (is (= 2 (count (filter #(= :close (:op %)) segs))))))
  (testing "smooth output is cubic and parses"
    (let [segs (p/parse-path (t/trace->path-d (mask-image 40 40 #(.fillOval % 5 5 30 30)) {:smooth? true :epsilon 1.5}))]
      (is (some #(= :cubic (:op %)) segs))
      (is (not-any? #(= :line (:op %)) segs))
      (is (= :close (:op (last segs)))))))

(deftest large-mask-performance-and-no-stack-overflow
  (let [img (mask-image 2000 2000 #(do (.fillOval % 50 50 1900 1900) (.setColor % Color/BLACK) (.fillOval % 600 600 800 800)))
        t0 (System/nanoTime)
        cs (t/trace-contours img {})
        ms (/ (- (System/nanoTime) t0) 1e6)]
    (is (= [false true] (mapv :hole? (sort-by :hole? cs))))
    (println "  trace 2000x2000 (4 MP) took" (long ms) "ms")))
