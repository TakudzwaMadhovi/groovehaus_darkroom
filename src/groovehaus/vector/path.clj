(ns groovehaus.vector.path
  "Pure-data SVG path support (no JavaFX): parser, serializer and curve conversions.

  `parse-path` turns a `d` string into a vector of absolute segments:
    {:op :move  :x :y}
    {:op :line  :x :y}
    {:op :quad  :x1 :y1 :x :y}
    {:op :cubic :x1 :y1 :x2 :y2 :x :y}
    {:op :arc   :rx :ry :rotation :large? :sweep? :x :y}   ; rotation in degrees
    {:op :close}
  Relative commands, H/V, S/T reflection and implicit command repetition are all resolved,
  so consumers only ever see absolute coordinates."
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def ^:private arity {\M 2 \L 2 \H 1 \V 1 \C 6 \S 4 \Q 4 \T 2 \A 7 \Z 0})

(def ^:private number-re #"[+-]?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?")

(defn- fail [d i msg]
  (throw (ex-info (str "Invalid SVG path: " msg " at index " i) {:d d :index i})))

(defn- upper ^Character [^Character c] (Character/toUpperCase c))

(defn parse-path
  "Parses an SVG path `d` string into absolute segments (see ns doc). Throws ex-info with
  :index on malformed input. Zero-radius arcs become lines, negative radii are made positive,
  as the SVG spec requires."
  [^String d]
  (let [n (.length d)
        pos (volatile! 0)
        m (re-matcher number-re d)
        skip! (fn []
                (loop []
                  (let [i (long @pos)]
                    (when (and (< i n)
                               (let [c (.charAt d i)] (or (Character/isWhitespace c) (= c \,))))
                      (vswap! pos inc)
                      (recur)))))
        num! (fn []
               (skip!)
               (.region m (int @pos) n)
               (if (.lookingAt m)
                 (do (vreset! pos (.end m)) (Double/parseDouble (.group m)))
                 (fail d @pos "expected number")))
        flag! (fn []
                (skip!)
                (let [i (long @pos)
                      c (when (< i n) (.charAt d i))]
                  (case c
                    \0 (do (vswap! pos inc) false)
                    \1 (do (vswap! pos inc) true)
                    (fail d i "expected arc flag 0 or 1"))))]
    (loop [segs [] cmd nil cx 0.0 cy 0.0 sx 0.0 sy 0.0 pc nil]
      (skip!)
      (if (>= (long @pos) n)
        segs
        (let [ch (.charAt d (int @pos))
              [cmd start?]
              (cond
                (Character/isLetter ch) (do (vswap! pos inc) [ch true])
                (nil? cmd) (fail d @pos "path must start with a command")
                (#{\Z \z} cmd) (fail d @pos "numbers after Z")
                (= cmd \M) [\L false]
                (= cmd \m) [\l false]
                :else [cmd false])
              up (upper cmd)
              rel? (Character/isLowerCase ^char cmd)]
          (when-not (contains? arity up) (fail d (dec (long @pos)) (str "unknown command " cmd)))
          (when (and (empty? segs) (not= up \M)) (fail d 0 "path must start with M"))
          (let [ox (if rel? cx 0.0) oy (if rel? cy 0.0)]
            (case up
              \Z (recur (conj segs {:op :close}) cmd sx sy sx sy nil)
              \M (let [x (+ ox (num!)) y (+ oy (num!))]
                   (recur (conj segs {:op :move :x x :y y}) cmd x y x y nil))
              \L (let [x (+ ox (num!)) y (+ oy (num!))]
                   (recur (conj segs {:op :line :x x :y y}) cmd x y sx sy nil))
              \H (let [x (+ ox (num!))]
                   (recur (conj segs {:op :line :x x :y cy}) cmd x cy sx sy nil))
              \V (let [y (+ oy (num!))]
                   (recur (conj segs {:op :line :x cx :y y}) cmd cx y sx sy nil))
              \C (let [x1 (+ ox (num!)) y1 (+ oy (num!)) x2 (+ ox (num!)) y2 (+ oy (num!))
                       x (+ ox (num!)) y (+ oy (num!))]
                   (recur (conj segs {:op :cubic :x1 x1 :y1 y1 :x2 x2 :y2 y2 :x x :y y})
                          cmd x y sx sy {:type :cubic :x x2 :y y2}))
              \S (let [[x1 y1] (if (= :cubic (:type pc))
                                 [(- (* 2 cx) (:x pc)) (- (* 2 cy) (:y pc))]
                                 [cx cy])
                       x2 (+ ox (num!)) y2 (+ oy (num!)) x (+ ox (num!)) y (+ oy (num!))]
                   (recur (conj segs {:op :cubic :x1 x1 :y1 y1 :x2 x2 :y2 y2 :x x :y y})
                          cmd x y sx sy {:type :cubic :x x2 :y y2}))
              \Q (let [x1 (+ ox (num!)) y1 (+ oy (num!)) x (+ ox (num!)) y (+ oy (num!))]
                   (recur (conj segs {:op :quad :x1 x1 :y1 y1 :x x :y y})
                          cmd x y sx sy {:type :quad :x x1 :y y1}))
              \T (let [[x1 y1] (if (= :quad (:type pc))
                                 [(- (* 2 cx) (:x pc)) (- (* 2 cy) (:y pc))]
                                 [cx cy])
                       x (+ ox (num!)) y (+ oy (num!))]
                   (recur (conj segs {:op :quad :x1 x1 :y1 y1 :x x :y y})
                          cmd x y sx sy {:type :quad :x x1 :y y1}))
              \A (let [rx (Math/abs ^double (num!)) ry (Math/abs ^double (num!)) rot (num!)
                       large? (flag!) sweep? (flag!)
                       x (+ ox (num!)) y (+ oy (num!))]
                   (recur (conj segs (if (or (zero? rx) (zero? ry))
                                       {:op :line :x x :y y}
                                       {:op :arc :rx rx :ry ry :rotation rot
                                        :large? large? :sweep? sweep? :x x :y y}))
                          cmd x y sx sy nil)))))))))

(defn- fmt [^double v]
  (let [s (str/replace (format "%.3f" v) #"\.?0+$" "")]
    (if (= s "-0") "0" s)))

(defn segments->d
  "Serializes absolute segments back to a path `d` string (3 decimal places)."
  [segs]
  (str/join
   " "
   (map (fn [{:keys [op x y x1 y1 x2 y2 rx ry rotation large? sweep?]}]
          (case op
            :move (str "M" (fmt x) " " (fmt y))
            :line (str "L" (fmt x) " " (fmt y))
            :quad (str "Q" (fmt x1) " " (fmt y1) " " (fmt x) " " (fmt y))
            :cubic (str "C" (fmt x1) " " (fmt y1) " " (fmt x2) " " (fmt y2) " " (fmt x) " " (fmt y))
            :arc (str "A" (fmt rx) " " (fmt ry) " " (fmt rotation) " " (if large? 1 0) " "
                      (if sweep? 1 0) " " (fmt x) " " (fmt y))
            :close "Z"))
        segs)))

(defn quad->cubic
  "Exact degree elevation of a quadratic segment starting at (x0,y0)."
  [x0 y0 {:keys [x1 y1 x y]}]
  {:op :cubic
   :x1 (+ x0 (* (/ 2.0 3.0) (- x1 x0))) :y1 (+ y0 (* (/ 2.0 3.0) (- y1 y0)))
   :x2 (+ x (* (/ 2.0 3.0) (- x1 x))) :y2 (+ y (* (/ 2.0 3.0) (- y1 y)))
   :x x :y y})

(defn arc->cubics
  "Converts an :arc segment starting at (x0,y0) to cubic segments of at most 90 degrees each
  (SVG 1.1 implementation notes F.6.5). Out-of-range radii are scaled up as the spec requires."
  [x0 y0 {:keys [rx ry rotation large? sweep? x y]}]
  (let [x0 (double x0) y0 (double y0) x (double x) y (double y)
        phi (Math/toRadians (double rotation))
        cp (Math/cos phi) sp (Math/sin phi)
        dx (/ (- x0 x) 2.0) dy (/ (- y0 y) 2.0)
        x1' (+ (* cp dx) (* sp dy))
        y1' (+ (* (- sp) dx) (* cp dy))
        lambda (+ (/ (* x1' x1') (* rx rx)) (/ (* y1' y1') (* ry ry)))
        s (if (> lambda 1.0) (Math/sqrt lambda) 1.0)
        rx (* s (double rx)) ry (* s (double ry))
        num (- (* rx rx ry ry) (* rx rx y1' y1') (* ry ry x1' x1'))
        den (+ (* rx rx y1' y1') (* ry ry x1' x1'))
        coef (* (if (= (boolean large?) (boolean sweep?)) -1.0 1.0)
                (Math/sqrt (Math/max 0.0 (/ num den))))
        cx' (* coef (/ (* rx y1') ry))
        cy' (* coef (- (/ (* ry x1') rx)))
        cx (+ (- (* cp cx') (* sp cy')) (/ (+ x0 x) 2.0))
        cy (+ (* sp cx') (* cp cy') (/ (+ y0 y) 2.0))
        th1 (Math/atan2 (/ (- y1' cy') ry) (/ (- x1' cx') rx))
        th2 (Math/atan2 (/ (- (- y1') cy') ry) (/ (- (- x1') cx') rx))
        dth (let [d (- th2 th1)]
              (cond (and sweep? (< d 0)) (+ d (* 2 Math/PI))
                    (and (not sweep?) (> d 0)) (- d (* 2 Math/PI))
                    :else d))
        n (max 1 (long (Math/ceil (- (/ (Math/abs (double dth)) (/ Math/PI 2)) 1e-9))))
        delta (/ dth n)
        alpha (* (/ 4.0 3.0) (Math/tan (/ delta 4.0)))
        pt (fn [px py] [(+ cx (- (* cp rx px) (* sp ry py))) (+ cy (* sp rx px) (* cp ry py))])]
    (vec
     (for [i (range n)
           :let [t (+ th1 (* i delta)) t2 (+ t delta)
                 [c1x c1y] (pt (- (Math/cos t) (* alpha (Math/sin t))) (+ (Math/sin t) (* alpha (Math/cos t))))
                 [c2x c2y] (pt (+ (Math/cos t2) (* alpha (Math/sin t2))) (- (Math/sin t2) (* alpha (Math/cos t2))))
                 [ex ey] (if (= i (dec n)) [x y] (pt (Math/cos t2) (Math/sin t2)))]]
       {:op :cubic :x1 c1x :y1 c1y :x2 c2x :y2 c2y :x ex :y ey}))))

(defn ->cubics
  "Rewrites quads and arcs as cubics so a path contains only :move/:line/:cubic/:close."
  [segs]
  (loop [[s & more] segs cx (identity 0.0) cy (identity 0.0) sx (identity 0.0) sy (identity 0.0) out []]
    (if-not s
      out
      (let [out' (case (:op s)
                   :quad (conj out (quad->cubic cx cy s))
                   :arc (into out (arc->cubics cx cy s))
                   (conj out s))
            [nx ny] (if (= :close (:op s)) [sx sy] [(:x s) (:y s)])
            [sx' sy'] (if (= :move (:op s)) [nx ny] [sx sy])]
        (recur more nx ny sx' sy' out')))))
