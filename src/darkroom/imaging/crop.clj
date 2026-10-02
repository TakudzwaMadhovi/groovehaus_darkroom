(ns darkroom.imaging.crop
  "The maths behind the interactive crop rectangle: moving it, dragging its
  handles, drawing a new one, and holding an aspect ratio. A rectangle is
  [x y w h] as fractions (0-1) of the frame. Pure logic, no UI dependency.")

(def min-size
  "Smallest width or height of a crop, as a fraction of the frame."
  0.03)

(defn- clamp [lo hi v] (max lo (min hi v)))

(defn lock-ratio
  "The w/h ratio a crop rectangle must keep *in fraction space*, or nil when the
  crop is free. `aspect` is a pixel ratio such as 1.5 (3:2), :orig for the
  frame's own shape, or nil/:free for no lock; `frame-aspect` is the frame's
  width / height."
  [aspect frame-aspect]
  (cond (or (nil? aspect) (= aspect :free)) nil
        (= aspect :orig) 1.0
        :else (/ (double aspect) (double frame-aspect))))

(defn full?
  "True when the rectangle covers the whole frame (to within a pixel's worth)."
  [[x y w h]]
  (and (< x 0.001) (< y 0.001) (> w 0.998) (> h 0.998)))

(defn move
  "The rectangle shifted by (dx, dy), kept inside the frame (it stops at the
  edges rather than shrinking)."
  [[x y w h] dx dy]
  [(clamp 0.0 (- 1.0 w) (+ x dx)) (clamp 0.0 (- 1.0 h) (+ y dy)) w h])

(defn- fit-ratio
  "Largest [w h] with w/h = `lock` no bigger than `w` x `h` and no bigger than
  the room `max-w` x `max-h`."
  [w h lock max-w max-h]
  (let [[w h] (if (> (/ w h) lock) [(* h lock) h] [w (/ w lock)])
        s (min 1.0 (/ max-w w) (/ max-h h))]
    [(* w s) (* h s)]))

(defn from-corners
  "A new crop from an anchor point and a moving point (both fractions): the
  rectangle they span, clamped to the frame, held to `lock` if given."
  [ax ay mx my lock]
  (let [ax (clamp 0.0 1.0 ax) ay (clamp 0.0 1.0 ay)
        mx (clamp 0.0 1.0 mx) my (clamp 0.0 1.0 my)
        sx (if (>= mx ax) 1.0 -1.0) sy (if (>= my ay) 1.0 -1.0)
        w  (max min-size (Math/abs (- mx ax))) h (max min-size (Math/abs (- my ay)))
        room-w (if (pos? sx) (- 1.0 ax) ax) room-h (if (pos? sy) (- 1.0 ay) ay)
        [w h] (if lock (fit-ratio w h lock room-w room-h) [(min w room-w) (min h room-h)])]
    [(if (pos? sx) ax (- ax w)) (if (pos? sy) ay (- ay h)) w h]))

(defn- extent
  "[lo hi] along one axis for a fixed end `f` and a dragged end `p`: at least
  `min-size` long, inside 0-1."
  [f p]
  (let [lo (min f p) hi (max f p)]
    (if (>= (- hi lo) min-size)
      [lo hi]
      (if (>= p f)
        (let [hi' (min 1.0 (+ f min-size))] [(- hi' min-size) hi'])
        (let [lo' (max 0.0 (- f min-size))] [lo' (+ lo' min-size)])))))

(defn resize
  "The rectangle after dragging `handle` (:nw :n :ne :e :se :s :sw :w) to the
  point (px, py). Corners keep the opposite corner fixed; edges keep the
  opposite edge fixed (and, with a `lock`, stay centred on the other axis).
  Always inside the frame and at least `min-size` in each direction; with a lock
  the ratio is exact."
  [[x y w h] handle px py lock]
  (let [x1 (+ x w) y1 (+ y h)
        px (clamp 0.0 1.0 px) py (clamp 0.0 1.0 py)]
    (case handle
      (:nw :ne :se :sw)
      (let [[ax ay] (case handle :nw [x1 y1] :ne [x y1] :se [x y] :sw [x1 y])]
        (from-corners ax ay px py lock))

      (:n :s)
      (let [f (if (= handle :n) y1 y)
            [ya yb] (extent f py)]
        (if lock
          (let [grow-down? (>= py f)
                room (if grow-down? (- 1.0 f) f)
                nh   (min (- yb ya) room (/ 1.0 lock))
                nw   (* nh lock)
                nx   (clamp 0.0 (- 1.0 nw) (- (+ x (/ w 2.0)) (/ nw 2.0)))]
            [nx (if grow-down? f (- f nh)) nw nh])
          [x ya w (- yb ya)]))

      (:w :e)
      (let [f (if (= handle :w) x1 x)
            [xa xb] (extent f px)]
        (if lock
          (let [grow-right? (>= px f)
                room (if grow-right? (- 1.0 f) f)
                nw   (min (- xb xa) room lock)
                nh   (/ nw lock)
                ny   (clamp 0.0 (- 1.0 nh) (- (+ y (/ h 2.0)) (/ nh 2.0)))]
            [(if grow-right? f (- f nw)) ny nw nh])
          [xa y (- xb xa) h])))))

(defn centered
  "The largest centred rectangle of pixel ratio `aspect` in a frame of
  `frame-aspect`, as fractions."
  [aspect frame-aspect]
  (let [lock (lock-ratio aspect frame-aspect)
        [w h] (if (> lock 1.0) [1.0 (/ 1.0 lock)] [lock 1.0])]
    [(/ (- 1.0 w) 2.0) (/ (- 1.0 h) 2.0) w h]))

(defn handle-at
  "Which handle (or :move / nil) is under the point (fractions), given the
  rectangle and the size in fractions that counts as 'on' a handle."
  [[x y w h] px py tol-x tol-y]
  (let [x1 (+ x w) y1 (+ y h)
        near-l? (<= (Math/abs (- px x)) tol-x) near-r? (<= (Math/abs (- px x1)) tol-x)
        near-t? (<= (Math/abs (- py y)) tol-y) near-b? (<= (Math/abs (- py y1)) tol-y)
        in-x? (<= (- x tol-x) px (+ x1 tol-x)) in-y? (<= (- y tol-y) py (+ y1 tol-y))]
    (cond (and near-t? near-l?) :nw
          (and near-t? near-r?) :ne
          (and near-b? near-r?) :se
          (and near-b? near-l?) :sw
          (and near-t? in-x?) :n
          (and near-b? in-x?) :s
          (and near-l? in-y?) :w
          (and near-r? in-y?) :e
          (and (< x px x1) (< y py y1)) :move
          :else nil)))
