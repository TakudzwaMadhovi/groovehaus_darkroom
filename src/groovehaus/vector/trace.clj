(ns groovehaus.vector.trace
  "Raster mask -> vector contours.

  Border following uses the Suzuki-Abe algorithm (1985), the Moore-neighbourhood tracer behind
  OpenCV's findContours: it finds every outer boundary *and* every hole boundary of 8-connected
  foreground regions. Raw boundaries are pixel-centre chains, which are then simplified with
  Ramer-Douglas-Peucker and emitted as SVG `d` strings (M x y L x y ... Z).

  Coordinates are image space with pixel (x,y) centred at (x+0.5, y+0.5). The traced polygon runs
  through boundary pixel *centres*, so it sits half a pixel inside the true mask edge. For a
  sub-pixel-accurate outline, trace a mask rendered at higher resolution and scale the path."
  (:require [clojure.string :as str])
  (:import [java.awt.image BufferedImage]))

(set! *warn-on-reflection* true)

;; ---- mask extraction -------------------------------------------------------

(defn mask
  "BufferedImage -> {:w :h :data boolean-like byte[]} (1 = foreground).
  If the image has any non-opaque pixel it is treated as an alpha cutout (foreground = alpha >=
  `threshold`, default 128). Otherwise it is a luminance mask (foreground = luma >= threshold,
  i.e. white on black). :invert? flips the result."
  [^BufferedImage img {:keys [threshold invert?] :or {threshold 128}}]
  (let [w (.getWidth img) h (.getHeight img)
        ^ints px (.getRGB img 0 0 w h nil 0 w)
        n (* w h)
        thr (long threshold)
        alpha? (loop [i 0] (cond (>= i n) false
                                 (not= 255 (bit-and (bit-shift-right (aget px i) 24) 0xff)) true
                                 :else (recur (inc i))))
        ^bytes data (byte-array n)]
    (dotimes [i n]
      (let [p (aget px i)
            v (if alpha?
                (bit-and (bit-shift-right p 24) 0xff)
                (long (+ (* 0.2126 (bit-and (bit-shift-right p 16) 0xff))
                         (* 0.7152 (bit-and (bit-shift-right p 8) 0xff))
                         (* 0.0722 (bit-and p 0xff)))))
            fg (>= v thr)]
        (aset data i (byte (if (if invert? (not fg) fg) 1 0)))))
    {:w w :h h :data data}))

;; ---- Suzuki-Abe border following -------------------------------------------

;; Neighbour directions, clockwise on screen (y down): E SE S SW W NW N NE.
(def ^:private ^"[I" DX (int-array [1 1 0 -1 -1 -1 0 1]))
(def ^:private ^"[I" DY (int-array [0 1 1 1 0 -1 -1 -1]))

(defn- dir-of ^long [^long dx ^long dy]
  (loop [d 0] (if (and (== dx (aget DX d)) (== dy (aget DY d))) d (recur (inc d)))))

(defn- follow-border
  "Traces one border starting at pixel (x,y). `f` is the (w2 x h2) padded label array. `sx,sy` is
  the already-examined zero neighbour to start the clockwise search from. Returns a vector of
  [x y] (padded coords) points."
  [^ints f w2 x y sx sy nbd]
  (let [w2 (long w2) x (long x) y (long y) sx (long sx) sy (long sy) nbd (long nbd)
        idx (fn ^long [^long px ^long py] (+ (* py w2) px))
        d0 (dir-of (- sx x) (- sy y))
        ;; 3.1: clockwise from the start neighbour, first non-zero
        first-nz (loop [k 0]
                   (when (< k 8)
                     (let [d (rem (+ d0 k) 8)
                           nx (+ x (aget DX d)) ny (+ y (aget DY d))]
                       (if (not (zero? (aget f (idx nx ny)))) [nx ny] (recur (inc k))))))]
    (if-not first-nz
      (do (aset f (idx x y) (int (- nbd))) [[x y]])
      (let [[x1 y1] first-nz]
        (loop [x2 x1 y2 y1 x3 (identity x) y3 (identity y) pts (transient [[x y]])]
          ;; 3.3: counter-clockwise from the next neighbour after (x2,y2) around (x3,y3)
          (let [dprev (dir-of (- x2 x3) (- y2 y3))
                [x4 y4 east-zero?]
                (loop [k 1 east-zero? false]
                  (let [d (mod (- dprev k) 8)
                        nx (+ x3 (aget DX d)) ny (+ y3 (aget DY d))]
                    (cond (not (zero? (aget f (idx nx ny)))) [nx ny east-zero?]
                          (> k 8) [x3 y3 east-zero?] ; unreachable for a non-isolated pixel
                          :else (recur (inc k) (or east-zero? (== d 0))))))]
            ;; 3.4
            (cond east-zero? (aset f (idx x3 y3) (int (- nbd)))
                  (== 1 (aget f (idx x3 y3))) (aset f (idx x3 y3) (int nbd)))
            ;; 3.5
            (if (and (== x4 x) (== y4 y) (== x3 x1) (== y3 y1))
              (persistent! pts)
              (recur x3 y3 x4 y4 (conj! pts [x4 y4])))))))))

(defn- area2
  "Twice the signed shoelace area of a closed polygon of [x y] points."
  ^double [pts]
  (let [v (vec pts) n (count v)]
    (loop [i 0 acc 0.0]
      (if (== i n)
        acc
        (let [[x1 y1] (v i) [x2 y2] (v (mod (inc i) n))]
          (recur (inc i) (+ acc (- (* (double x1) (double y2)) (* (double x2) (double y1))))))))))

(defn- perp-dist ^double [[px py] [ax ay] [bx by]]
  (let [dx (- (double bx) (double ax)) dy (- (double by) (double ay))
        len2 (+ (* dx dx) (* dy dy))]
    (if (zero? len2)
      (Math/hypot (- (double px) (double ax)) (- (double py) (double ay)))
      (/ (Math/abs (- (* dx (- (double ay) (double py))) (* (- (double ax) (double px)) dy)))
         (Math/sqrt len2)))))

(defn- rdp-open
  "Ramer-Douglas-Peucker on a vector of points, keeping both endpoints. Iterative."
  [pts ^double eps]
  (let [n (count pts)]
    (if (< n 3)
      pts
      (let [keep (boolean-array n)]
        (aset keep 0 true) (aset keep (dec n) true)
        (loop [stack [[0 (dec n)]]]
          (when (seq stack)
            (let [[lo hi] (peek stack) stack (pop stack)]
              (if (< (- hi lo) 2)
                (recur stack)
                (let [[idx dmax] (reduce (fn [[bi bd] i]
                                           (let [d (perp-dist (pts i) (pts lo) (pts hi))]
                                             (if (> d bd) [i d] [bi bd])))
                                         [lo -1.0] (range (inc lo) hi))]
                  (if (> (double dmax) eps)
                    (do (aset keep idx true) (recur (conj stack [lo idx] [idx hi])))
                    (recur stack)))))))
        (vec (keep-indexed (fn [i p] (when (aget keep i) p)) pts))))))

(defn simplify-closed
  "RDP for a closed polygon: splits at the point farthest from the first so the seam is stable."
  [pts eps]
  (let [v (vec pts) n (count v)]
    (if (or (< n 4) (<= (double eps) 0.0))
      v
      (let [p0 (v 0)
            far (apply max-key #(let [[x y] (v %) [x0 y0] p0] (Math/hypot (- (double x) (double x0)) (- (double y) (double y0))))
                       (range n))
            a (rdp-open (subvec v 0 (inc far)) eps)
            b (rdp-open (conj (subvec v far) p0) eps)]
        (vec (concat (butlast a) (butlast b)))))))

(defn trace-mask
  "Suzuki-Abe on a mask ({:w :h :data}). Returns raw contours:
  [{:points [[x y] ...] :hole? bool} ...] in pixel coordinates (not yet offset by 0.5)."
  [{:keys [^long w ^long h ^bytes data]}]
  (let [w2 (+ w 2) h2 (+ h 2)
        ^ints f (int-array (* w2 h2))]
    (dotimes [y h]
      (dotimes [x w]
        (when (== 1 (aget data (+ (* y w) x)))
          (aset f (+ (* (+ y 1) w2) (+ x 1)) 1))))
    (loop [y 1 nbd (identity 1) out (transient [])]
      (if (> y h)
        (persistent! out)
        (let [[nbd out]
              (loop [x 1 nbd nbd out out]
                (if (> x w)
                  [nbd out]
                  (let [v (aget f (+ (* y w2) x))]
                    (cond
                      (zero? v) (recur (inc x) nbd out)
                      (and (== v 1) (zero? (aget f (+ (* y w2) (dec x)))))
                      (let [nbd (inc nbd)
                            pts (follow-border f w2 x y (dec x) y nbd)]
                        (recur (inc x) nbd (conj! out {:points (mapv (fn [[px py]] [(dec px) (dec py)]) pts) :hole? false})))
                      (and (>= v 1) (zero? (aget f (+ (* y w2) (inc x)))))
                      (let [nbd (inc nbd)
                            pts (follow-border f w2 x y (inc x) y nbd)]
                        (recur (inc x) nbd (conj! out {:points (mapv (fn [[px py]] [(dec px) (dec py)]) pts) :hole? true})))
                      :else (recur (inc x) nbd out)))))]
          (recur (inc y) nbd out))))))

(defn trace-contours
  "Binary/alpha mask BufferedImage -> simplified contours [{:points [[x y]...] :hole? :area}].
  Options:
    :threshold  mask threshold 0..255 (default 128); :invert? flip foreground/background
    :epsilon    RDP tolerance in pixels (default 1.0; 0 keeps every boundary pixel)
    :min-area   drop contours enclosing less than this many px^2 (default 1.0, which removes
                isolated pixels and 1px-wide hairlines; use 0 to keep everything)"
  [^BufferedImage img {:keys [epsilon min-area] :or {epsilon 1.0 min-area 1.0} :as opts}]
  (->> (trace-mask (mask img opts))
       (keep (fn [{:keys [points hole?]}]
               (let [pts (->> (simplify-closed points epsilon)
                              (mapv (fn [[x y]] [(+ 0.5 (double x)) (+ 0.5 (double y))])))
                     area (/ (Math/abs (area2 pts)) 2.0)]
                 (when (and (>= (count pts) 3) (>= area (double min-area)))
                   {:points pts :hole? hole? :area area}))))
       vec))

;; ---- SVG output ---------------------------------------------------------------

(defn- fmt [^double v]
  (let [s (str/replace (format "%.2f" v) #"\.?0+$" "")] (if (= s "-0") "0" s)))

(defn- smooth-subpath
  "Closed Catmull-Rom spline through the points as cubic Beziers."
  [pts]
  (let [v (vec pts) n (count v)
        at (fn [i] (v (mod i n)))]
    (str "M" (fmt (ffirst v)) " " (fmt (second (first v))) " "
         (str/join " "
                   (for [i (range n)
                         :let [[x0 y0] (at (dec i)) [x1 y1] (at i) [x2 y2] (at (inc i)) [x3 y3] (at (+ i 2))
                               c1x (+ x1 (/ (- x2 x0) 6.0)) c1y (+ y1 (/ (- y2 y0) 6.0))
                               c2x (- x2 (/ (- x3 x1) 6.0)) c2y (- y2 (/ (- y3 y1) 6.0))]]
                     (str "C" (fmt c1x) " " (fmt c1y) " " (fmt c2x) " " (fmt c2y) " " (fmt x2) " " (fmt y2))))
         " Z")))

(defn contours->path-d
  "Contours -> a single SVG path `d` string, one subpath per contour. Render with
  :fill-rule :even-odd so holes cut through. :smooth? true emits Catmull-Rom cubic curves
  instead of straight segments (rounds corners)."
  ([contours] (contours->path-d contours nil))
  ([contours {:keys [smooth?]}]
   (str/join
    " "
    (for [{:keys [points]} contours]
      (if smooth?
        (smooth-subpath points)
        (str "M" (fmt (ffirst points)) " " (fmt (second (first points)))
             " " (str/join " " (map (fn [[x y]] (str "L" (fmt x) " " (fmt y))) (rest points)))
             " Z"))))))

(defn trace->path-d
  "BufferedImage mask -> SVG path `d` string. See trace-contours / contours->path-d options."
  ([img] (trace->path-d img nil))
  ([img opts] (contours->path-d (trace-contours img opts) opts)))
