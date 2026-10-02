(ns darkroom.imaging.panorama
  "Panoramas from overlapping frames, merged in linear light on the editor's
  float scene images (so a RAW panorama keeps its range).

    1. AKAZE features on a reduced grey copy of every frame; each pair of frames
       is matched (nearest-neighbour ratio test) and related by a homography found
       with RANSAC. Frames need roughly 20-30% overlap and some texture.
    2. The best-connected frame is the reference; the others are chained to it
       through the strongest links (a maximum spanning tree of the match graph),
       so frames need not all overlap the reference directly.
    3. Gains are solved for the overlaps (Brown & Lowe's gain compensation) so
       exposure differences do not show as seams.
    4. Frames are warped onto one canvas and blended with weights that fall to
       zero at each frame's border (feathering).
    5. The largest rectangle fully covered by frames is reported as a crop.

  Planar homographies suit panoramas of modest field of view and flat or distant
  scenes; very wide cylindrical or spherical panoramas, or scenes with strong
  parallax, will show misalignment (there is no cylindrical projection or
  bundle adjustment). Needs OpenCV's feature modules; on Linux they need the GTK 2
  libraries installed (libgtk2.0-0). Pure logic, no UI dependency."
  (:require [darkroom.imaging.color :as color]
            [darkroom.imaging.core :as core]
            [darkroom.imaging.scene :as scene])
  (:import (org.bytedeco.javacpp DoublePointer FloatPointer)
           (org.bytedeco.opencv.global opencv_calib3d opencv_core opencv_imgproc opencv_video)
           (org.bytedeco.opencv.opencv_core DMatch DMatchVectorVector KeyPointVector Mat Rect Scalar Size TermCriteria)
           (org.bytedeco.opencv.opencv_features2d AKAZE BFMatcher)))

(def ^:private feature-side 1600)
(def ^:private min-inliers 15)

;; ------------------------------------------------------------------ helpers

(defn- luma ^double [^floats d ^long i]
  (/ (+ (aget d (* 3 i)) (aget d (+ (* 3 i) 1)) (aget d (+ (* 3 i) 2))) 3.0))

(defn- gray8-mat
  "8-bit grey Mat of the perceptual brightness of a scene image."
  ^Mat [{:keys [^long width ^long height data]}]
  (let [^floats d data n (* width height) ^bytes px (byte-array n)
        m (Mat. (int height) (int width) opencv_core/CV_8UC1)]
    (dotimes [i n]
      (aset px i (unchecked-byte (Math/round (* 255.0 (Math/max 0.0 (Math/min 1.0 (scene/srgb-encode-extended (luma d i)))))))))
    (.put (.data m) px)
    m))

(defn- mat3 ^Mat [h]
  (let [m (Mat. 3 3 opencv_core/CV_64F)]
    (.put (DoublePointer. (.data m)) (double-array h))
    m))

(defn- ->h [^Mat m] (let [a (double-array 9)] (.get (DoublePointer. (.data m)) a) (vec a)))

(defn- apply-h [h x y]
  (let [[a b c d e f g hh i] (map double h)
        w (+ (* g x) (* hh y) i)]
    [(/ (+ (* a x) (* b y) c) w) (/ (+ (* d x) (* e y) f) w)]))

(defn- scale-h [s-out h s-in]
  "diag(s-out) . h . diag(1/s-in) for homography `h` (9 numbers)."
  (let [[a b c d e f g hh i] (map double h)
        so (double s-out) si (double s-in)]
    [(* so a (/ 1.0 si)) (* so b (/ 1.0 si)) (* so c)
     (* so d (/ 1.0 si)) (* so e (/ 1.0 si)) (* so f)
     (/ g si) (/ hh si) i]))

;; ------------------------------------------------------------------ matching

(defn- gray32-mat
  "Float grey Mat (0-1, perceptual) of a scene image, for the sub-pixel refinement."
  ^Mat [{:keys [^long width ^long height data]}]
  (let [^floats d data n (* width height) ^floats px (float-array n)
        m (Mat. (int height) (int width) opencv_core/CV_32FC1)]
    (dotimes [i n] (aset px i (float (Math/max 0.0 (Math/min 1.0 (scene/srgb-encode-extended (luma d i)))))))
    (.put (FloatPointer. (.data m)) px)
    m))

(defn- detect
  "AKAZE keypoints (as float x,y arrays), descriptors and a float grey copy of a scene image."
  [img]
  (let [g (gray8-mat img) akaze (AKAZE/create) kps (KeyPointVector.) desc (Mat.)]
    (try
      (.detectAndCompute akaze g (Mat.) kps desc)
      (let [n (.size kps) pts (float-array (* 2 n))]
        (dotimes [i n] (let [p (.pt (.get kps i))] (aset pts (* 2 i) (.x p)) (aset pts (inc (* 2 i)) (.y p))))
        {:pts pts :desc desc :n n :gray (gray32-mat img)})
      (finally (.close g)))))

(defn- refine
  "Sub-pixel refinement of homography `h` (frame a -> frame b) by maximising the
  enhanced correlation coefficient of the two grey images (ECC); `h` itself when
  the iteration fails to converge."
  [h fa fb]
  (let [warp (Mat. 3 3 opencv_core/CV_32F)] ; ECC wants a single-precision warp
    (try
      (.put (FloatPointer. (.data warp)) (float-array (map float h)))
      (opencv_video/findTransformECC ^Mat (:gray fa) ^Mat (:gray fb) warp opencv_video/MOTION_HOMOGRAPHY
                                     (TermCriteria. (bit-or TermCriteria/COUNT TermCriteria/EPS) 80 1e-7) (Mat.) 5)
      (let [a (float-array 9)]
        (.get (FloatPointer. (.data warp)) a)
        (let [r (mapv double a)]
          ;; accept only a small correction of the RANSAC estimate (a runaway ECC is worse than none)
          (if (and (every? #(Double/isFinite %) r)
                   (< (Math/abs (- (double (r 2)) (double (h 2)))) 8.0) (< (Math/abs (- (double (r 5)) (double (h 5)))) 8.0))
            r h)))
      (catch Throwable _ h)
      (finally (.close warp)))))

(defn- relate
  "{:h [9 numbers] :inliers n}: the homography taking points of frame `a`
  (features `fa`) to frame `b`, or nil when fewer than `min-inliers` matches agree."
  [fa fb]
  (when (and (> (long (:n fa)) 8) (> (long (:n fb)) 8))
    (let [bf (BFMatcher. opencv_core/NORM_HAMMING false) mm (DMatchVectorVector.)]
      (.knnMatch bf ^Mat (:desc fa) ^Mat (:desc fb) mm 2)
      (let [good (vec (for [i (range (.size mm)) :let [m (.get mm i)]
                            :when (and (== 2 (.size m)) (< (.distance (.get m 0)) (* 0.75 (.distance (.get m 1)))))]
                        [(.queryIdx (.get m 0)) (.trainIdx (.get m 0))]))
            n (count good)]
        (when (>= n min-inliers)
          (let [^floats pa (:pts fa) ^floats pb (:pts fb)
                sa (float-array (* 2 n)) da (float-array (* 2 n))]
            (doseq [[k [qi ti]] (map-indexed vector good)]
              (aset sa (* 2 k) (aget pa (* 2 qi))) (aset sa (inc (* 2 k)) (aget pa (inc (* 2 qi))))
              (aset da (* 2 k) (aget pb (* 2 ti))) (aset da (inc (* 2 k)) (aget pb (inc (* 2 ti)))))
            (let [src (Mat. (int n) 1 opencv_core/CV_32FC2) dst (Mat. (int n) 1 opencv_core/CV_32FC2) mask (Mat.)]
              (try
                (.put (FloatPointer. (.data src)) sa)
                (.put (FloatPointer. (.data dst)) da)
                (let [h (opencv_calib3d/findHomography src dst opencv_calib3d/RANSAC 3.0 mask 2000 0.995)]
                  (when-not (.empty h)
                    (let [inl (opencv_core/countNonZero mask)]
                      (when (>= inl min-inliers) {:h (refine (->h h) fa fb) :inliers inl}))))
                (finally (.close src) (.close dst) (.close mask))))))))))

(defn- spanning-tree
  "Maximum spanning tree of the match graph from the best-connected frame.
  `links` {[i j] {:h (i -> j) :inliers n}}. Returns {:ref r :to-ref {i H(i -> ref)}}
  or throws naming the frames that do not connect."
  [n links]
  (let [strength (fn [i] (reduce + (for [[[a b] l] links :when (or (== a i) (== b i))] (:inliers l))))
        ref (apply max-key strength (range n))
        edge (fn [i j] (if-let [l (links [i j])] [(:h l) (:inliers l)]
                         (when-let [l (links [j i])] [(color/mat-inv (:h l)) (:inliers l)])))]
    (loop [done {ref [1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0]}]
      (if (== (count done) n)
        {:ref ref :to-ref done}
        (let [cands (for [i (keys done) j (range n) :when (not (done j))
                          :let [[h inl] (edge j i)] :when h]   ; h takes j's points into i's frame
                      [inl i j h])]
          (when (empty? cands)
            (throw (ex-info "Some frames do not overlap the others enough to be joined"
                            {:unconnected (vec (remove done (range n)))})))
          (let [[_ i j h] (apply max-key first cands)]
            (recur (assoc done j (color/mat* (done i) h)))))))))

;; --------------------------------------------------------------- gains

(defn- eliminate-upper
  "Back-substitution on an upper-triangular augmented matrix `m` (n rows, n+1 columns)."
  [m n]
  (let [x (double-array n)]
    (doseq [i (range (dec n) -1 -1)]
      (aset x i (/ (- (double (get-in m [i n])) (reduce + (for [j (range (inc i) n)] (* (double (get-in m [i j])) (aget x j)))))
                   (double (get-in m [i i])))))
    (vec x)))

(defn gains
  "Per-frame gains from overlap statistics (Brown & Lowe): `overlaps`
  {[i j] {:n pixels :mean-i mean of frame i in the overlap :mean-j mean of frame j}}.
  Frames with no overlaps keep gain 1."
  [n overlaps]
  (let [sn2 (Math/pow (/ 10.0 255.0) 2.0) sg2 (Math/pow 0.1 2.0)
        A (vec (for [_ (range n)] (vec (repeat n 0.0))))
        b (vec (repeat n 0.0))
        [A b] (reduce (fn [[A b] [[i j] {:keys [n mean-i mean-j]}]]
                        (let [n (double n) mi (double mean-i) mj (double mean-j)]
                          [(-> A
                               (update-in [i i] + (* n (+ (/ (* mi mi) sn2) (/ 1.0 sg2))))
                               (update-in [j j] + (* n (+ (/ (* mj mj) sn2) (/ 1.0 sg2))))
                               (update-in [i j] - (/ (* n mi mj) sn2))
                               (update-in [j i] - (/ (* n mi mj) sn2)))
                           (-> b (update i + (/ n sg2)) (update j + (/ n sg2)))]))
                      [A b] overlaps)
        ;; frames without any overlap: identity row
        A (vec (map-indexed (fn [i row] (if (zero? (double (get b i))) (assoc row i 1.0) row)) A))
        b (vec (map-indexed (fn [i v] (if (zero? (double v)) 1.0 v)) b))]
    (let [m (mapv (fn [row bi] (conj (vec row) bi)) A b)
          ;; forward elimination
          m (loop [k 0 m m]
              (if (== k n) m
                  (let [p (apply max-key #(Math/abs (double (get-in m [% k]))) (range k n))
                        m (-> m (assoc k (m p)) (assoc p (m k)))
                        piv (double (get-in m [k k]))
                        m (vec (map-indexed (fn [i row]
                                              (if (> i k)
                                                (let [f (/ (double (row k)) piv)]
                                                  (vec (map-indexed (fn [c v] (- (double v) (* f (double ((m k) c))))) row)))
                                                row))
                                            m))]
                    (recur (inc k) m))))]
      (eliminate-upper m n))))

;; ------------------------------------------------------------ the canvas

(defn- weight-mat
  "Float Mat (h x w) of each pixel's distance to the nearest frame edge, 0-1,
  squared: the feathering weight."
  ^Mat [^long w ^long h]
  (let [^floats a (float-array (* w h)) m (Mat. (int h) (int w) opencv_core/CV_32FC1)]
    (dotimes [y h]
      (let [wy (/ (+ 0.5 (Math/min y (- h 1 y))) (* 0.5 h))]
        (dotimes [x w]
          (let [wx (/ (+ 0.5 (Math/min x (- w 1 x))) (* 0.5 w))
                v (Math/min 1.0 (* wx wy 2.0))]
            (aset a (+ (* y w) x) (float (* v v)))))))
    (.put (FloatPointer. (.data m)) a)
    m))

(defn- warp
  "Warps float Mat `src` with homography `h` into a Mat of `w` x `h` (type `type`), zero outside."
  ^Mat [^Mat src h w hh type]
  (let [dst (Mat.) m (mat3 h)]
    (try
      (opencv_imgproc/warpPerspective src dst m (Size. (int w) (int hh)) opencv_imgproc/INTER_LINEAR opencv_core/BORDER_CONSTANT (Scalar. 0.0))
      dst
      (finally (.close m)))))

(defn- float-mat ^Mat [{:keys [^long width ^long height data]}]
  (let [m (Mat. (int height) (int width) opencv_core/CV_32FC3)]
    (.put (FloatPointer. (.data m)) ^floats data)
    m))

(defn- mat-floats ^floats [^Mat m n]
  (let [a (float-array n)] (.get (FloatPointer. (.data m)) a) a))

(defn largest-rectangle
  "[x y w h] (cells) of the largest axis-aligned rectangle of true cells in the
  boolean grid `valid` (row-major, `cols` x `rows`)."
  [^booleans valid ^long cols ^long rows]
  (let [heights (long-array cols)]
    (loop [y 0 best [0 0 0 0] best-area 0]
      (if (== y rows)
        best
        (do
          (dotimes [x cols] (aset heights x (if (aget valid (+ (* y cols) x)) (inc (aget heights x)) 0)))
          ;; largest rectangle in a histogram (stack)
          (let [[b a] (loop [x 0 stack [] best best best-area best-area]
                        (if (> x cols)
                          [best best-area]
                          (let [h (if (== x cols) 0 (aget heights x))]
                            (let [[stack best best-area]
                                  (loop [stack stack best best best-area best-area]
                                    (if (and (seq stack) (>= (long (aget heights (peek stack))) h))
                                      (let [top (peek stack) st (pop stack)
                                            height (aget heights top)
                                            left (if (seq st) (inc (long (peek st))) 0)
                                            width (- x left)
                                            area (* height width)]
                                        (recur st (if (> area best-area) [left (inc (- y height)) width height] best) (Math/max (long area) (long best-area))))
                                      [stack best best-area]))]
                              (recur (inc x) (conj stack x) best best-area)))))]
            (recur (inc y) b a)))))))

(defn panorama
  "Joins `imgs` (float scene images, one per frame) into a panorama. opts
  :max-pixels (default 60 million) limits the canvas, :progress (fn [done total])
  is called as the work advances. Returns {:scene :valid-rect [x y w h] as
  fractions of the scene :reference :homographies :gains :scale}; throws ex-info when
  frames cannot be joined."
  [imgs & [{:keys [max-pixels progress] :or {max-pixels 60000000}}]]
  (let [n (count imgs)
        _ (when (< n 2) (throw (ex-info "Need at least two frames" {})))
        tick (let [done (atom 0)] (fn [] (when progress (progress (swap! done inc) (+ n (* n (dec n) 1/2) n)))))
        reduced (mapv #(scene/fit % feature-side) imgs)
        ks (mapv (fn [full red] (/ (double (:width full)) (double (:width red)))) imgs reduced)
        feats (mapv (fn [r] (let [f (detect r)] (tick) f)) reduced)
        links (into {} (for [i (range n) j (range (inc i) n)
                             :let [l (do (tick) (relate (feats i) (feats j)))]
                             :when l]
                         [[i j] l]))
        {:keys [ref to-ref]} (spanning-tree n links)
        ;; homographies in full-resolution pixel coordinates, frame -> reference
        hs (mapv (fn [i] (scale-h (ks ref) (to-ref i) (ks i))) (range n))
        corners (fn [i] (let [w (double (:width (imgs i))) h (double (:height (imgs i)))]
                          (map #(apply-h (hs i) (first %) (second %)) [[0.0 0.0] [w 0.0] [w h] [0.0 h]])))
        all (mapcat corners (range n))
        minx (apply min (map first all)) maxx (apply max (map first all))
        miny (apply min (map second all)) maxy (apply max (map second all))
        raw-w (- maxx minx) raw-h (- maxy miny)
        scale (Math/min 1.0 (Math/sqrt (/ (double max-pixels) (* raw-w raw-h))))
        cw (long (Math/ceil (* scale raw-w))) ch (long (Math/ceil (* scale raw-h)))
        ;; frame -> canvas: translate to the canvas origin, then scale
        to-canvas (fn [h] (color/mat* [scale 0.0 0.0 0.0 scale 0.0 0.0 0.0 1.0]
                                      (color/mat* [1.0 0.0 (- minx) 0.0 1.0 (- miny) 0.0 0.0 1.0] h)))
        hc (mapv #(to-canvas (hs %)) (range n))
        ;; overlap statistics on a small canvas
        gs (/ 400.0 (Math/max cw ch))
        sw (Math/max 2 (long (* gs cw))) sh (Math/max 2 (long (* gs ch)))
        small-h (mapv (fn [h] (color/mat* [gs 0.0 0.0 0.0 gs 0.0 0.0 0.0 1.0] (scale-h 1.0 h 1.0))) hc)
        small (mapv (fn [i]
                      (let [sm (scene/fit (imgs i) 800) k (/ (double (:width (imgs i))) (double (:width sm)))
                            m (float-mat sm) wm (weight-mat (:width sm) (:height sm))
                            h (scale-h 1.0 (small-h i) (/ 1.0 k)) ; small-image pixels -> full -> small canvas
                            im (warp m h sw sh opencv_core/CV_32FC3) wt (warp wm h sw sh opencv_core/CV_32FC1)
                            r {:img (mat-floats im (* 3 sw sh)) :w (mat-floats wt (* sw sh))}]
                        (.close m) (.close wm) (.close im) (.close wt) r))
                    (range n))
        overlaps (into {} (for [i (range n) j (range (inc i) n)
                                :let [a (small i) b (small j)
                                      [cnt sa sb] (reduce (fn [[c sa sb] p]
                                                            (if (and (> (aget ^floats (:w a) p) 0.05) (> (aget ^floats (:w b) p) 0.05))
                                                              [(inc c) (+ sa (luma (:img a) p)) (+ sb (luma (:img b) p))]
                                                              [c sa sb]))
                                                          [0 0.0 0.0] (range (* sw sh)))]
                                :when (> cnt 50)]
                            [[i j] {:n cnt :mean-i (/ sa cnt) :mean-j (/ sb cnt)}]))
        g (gains n overlaps)
        ;; blend
        ^floats acc (float-array (* 3 cw ch)) ^floats wsum (float-array (* cw ch))]
    (doseq [i (range n)]
      (let [img (imgs i) h (hc i)
            ;; bounding box of the frame on the canvas
            pts (map #(apply-h h (first %) (second %)) [[0.0 0.0] [(double (:width img)) 0.0] [(double (:width img)) (double (:height img))] [0.0 (double (:height img))]])
            x0 (Math/max 0 (long (Math/floor (apply min (map first pts))))) y0 (Math/max 0 (long (Math/floor (apply min (map second pts)))))
            x1 (Math/min cw (long (Math/ceil (apply max (map first pts))))) y1 (Math/min ch (long (Math/ceil (apply max (map second pts)))))
            bw (- x1 x0) bh (- y1 y0)]
        (when (and (pos? bw) (pos? bh))
          (let [hb (color/mat* [1.0 0.0 (- x0) 0.0 1.0 (- y0) 0.0 0.0 1.0] h)
                m (float-mat img) wm (weight-mat (:width img) (:height img))
                im (warp m hb bw bh opencv_core/CV_32FC3) wt (warp wm hb bw bh opencv_core/CV_32FC1)
                ^floats pi (mat-floats im (* 3 bw bh)) ^floats pw (mat-floats wt (* bw bh))
                gi (double (g i))]
            (core/parallel-ranges!
              bh
              (fn [^long ya ^long yb]
                (loop [y ya]
                  (when (< y yb)
                    (dotimes [x bw]
                      (let [w (double (aget pw (+ (* y bw) x)))]
                        (when (> w 1e-6)
                          (let [o (+ (* (+ y0 y) cw) x0 x) k (* 3 (+ (* y bw) x))]
                            (aset wsum o (float (+ (aget wsum o) w)))
                            (dotimes [c 3] (aset acc (+ (* 3 o) c) (float (+ (aget acc (+ (* 3 o) c)) (* w gi (aget pi (+ k c)))))))))))
                    (recur (inc y))))))
            (.close m) (.close wm) (.close im) (.close wt)))
        (tick)))
    ;; normalise; build the validity grid for the crop
    (let [^floats out (float-array (* 3 cw ch))
          step (Math/max 1 (long (/ (Math/max cw ch) 300)))
          cols (quot cw step) rows (quot ch step)
          ^booleans valid (boolean-array (* cols rows))]
      (dotimes [i (* cw ch)]
        (let [w (aget wsum i)]
          (when (> w 0.0)
            (dotimes [c 3] (aset out (+ (* 3 i) c) (float (/ (aget acc (+ (* 3 i) c)) w)))))))
      (dotimes [y rows] (dotimes [x cols] (aset valid (+ (* y cols) x) (> (aget wsum (+ (* (* y step) cw) (* x step))) 0.0))))
      (let [[rx ry rw rh] (largest-rectangle valid cols rows)]
        {:scene (scene/image cw ch out)
         :valid-rect (when (pos? rw) [(/ (* rx step) (double cw)) (/ (* ry step) (double ch)) (/ (* rw step) (double cw)) (/ (* rh step) (double ch))])
         :reference ref :gains g :scale scale :homographies hc}))))
