(ns darkroom.imaging.local
  "Local adjustments: a list of layers, each a mask (linear or radial gradient,
  brush, range, subject or sky; optionally limited by a luminance and/or colour
  range, inverted and feathered) with its own tone and detail adjustments.
  Each layer's adjusted copy of the picture is blended in through its mask.
  Layers apply in order on the picture as edited so far, so range masks see
  the edited colours. Pure logic, no UI dependency.

  A layer is
    {:id n :type :linear|:radial|:brush|:range|:subject|:sky :visible true
     :shape {...}     ; linear {:x0 :y0 :x1 :y1}; radial {:cx :cy :rx :ry :feather};
                      ; brush {:strokes [...]}; subject {:rect [x y w h]}
     :range {:luma {:lo :hi :smooth} :color {:hue :range}}   ; optional limits
     :invert false :feather 0.0 :amount 1.0
     :adj {:exposure ...}}   ; see `adjust-keys`
  with coordinates as fractions of the picture, which is the *final* (cropped,
  turned) one, so set the crop before painting masks."
  (:require [darkroom.imaging.core :as core]
            [darkroom.imaging.detail :as detail]
            [darkroom.imaging.develop :as develop]
            [darkroom.imaging.mask :as mask]
            [darkroom.imaging.scene :as scene]))

(set! *unchecked-math* :warn-on-boxed)

(def defaults {:local []})

(def tone-adjust-keys
  [:exposure :contrast :highlights :shadows :whites :blacks :temp :tint :saturation :vibrance])

(def detail-adjust-keys [:texture :clarity :sharpen])

(def adjust-keys (into tone-adjust-keys detail-adjust-keys))

(defn- active-adjustments
  "{:tone {...} :detail {...}} with only the non-zero adjustments, or nil."
  [layer]
  (let [adj (:adj layer)
        nz  (fn [ks] (into {} (filter (fn [[_ v]] (and v (not (zero? (double v))))) (select-keys adj ks))))
        tone (nz tone-adjust-keys) det (nz detail-adjust-keys)]
    (when (or (seq tone) (seq det)) {:tone tone :detail det})))

(defn layer-active?
  "True for a visible layer that has an effect to apply."
  [layer]
  (and (not (false? (:visible layer)))
       (pos? (double (get layer :amount 1.0)))
       (some? (active-adjustments layer))))

(defn local-neutral? [settings] (not-any? layer-active? (:local settings)))

;; --------------------------------------------------------------------- masks

(defonce ^:private mask-cache (atom {:order [] :masks {}}))
(def ^:private ^:const max-cached 8)

(defn- cached
  "The mask for `key`, computing it with `f` (a no-arg fn) on a miss."
  [key f]
  (or (get-in @mask-cache [:masks key])
      (let [m (f)]
        (swap! mask-cache
               (fn [{:keys [order masks]}]
                 (let [order (conj (vec (remove #{key} order)) key)
                       drop-n (max 0 (- (long (count order)) (long max-cached)))
                       gone (take drop-n order)]
                   {:order (vec (drop drop-n order))
                    :masks (apply dissoc (assoc masks key m) gone)})))
        m)))

(defn- scene-dependent? [{:keys [type range]}]
  (boolean (or (#{:range :subject :sky} type) (seq range))))

(defn- base-mask ^floats [{:keys [width height] :as img} {:keys [type shape]}]
  (let [w (long width) h (long height)]
    (case type
      :linear  (mask/linear w h shape)
      :radial  (mask/radial w h shape)
      :brush   (mask/brush w h (:strokes shape))
      :subject (mask/subject img shape)
      :sky     (mask/sky img shape)
      :range   (mask/ones w h))))

(defn- times! [^floats a ^floats b]
  (dotimes [i (alength a)] (aset a i (float (* (aget a i) (aget b i)))))
  a)

(defn layer-mask
  "The layer's mask over `img`: the base shape, limited by its range, inverted
  if asked, feathered, as a float plane (0-1) the size of the image. The result
  may be shared (cached): do not modify it."
  ^floats [{:keys [^long width ^long height data] :as img} {:keys [range invert feather] :as layer}]
  (let [key [(when (scene-dependent? layer) (System/identityHashCode data)) width height
             (select-keys layer [:type :shape :range :invert :feather])]]
    (cached key
            (fn []
              (let [^floats m (aclone (base-mask img layer))
                    _ (when-let [luma (:luma range)] (times! m (mask/luminance img luma)))
                    _ (when-let [col (:color range)] (times! m (mask/hue-range img col)))
                    ^floats m (if invert
                                (let [o (float-array (alength m))]
                                  (dotimes [i (alength m)] (aset o i (float (- 1.0 (aget m i)))))
                                  o)
                                m)
                    f (double (or feather 0.0))]
                (if (pos? f)
                  (detail/blur-plane m width height (* f 0.02 (double (max width height))))
                  m))))))

;; ------------------------------------------------------------------ applying

(defn- blend
  "base + (adjusted - base) * mask * amount."
  [{:keys [width height data]} {adjusted :data} ^floats m ^double amount]
  (let [^floats a data ^floats b adjusted
        ^floats out (float-array (alength a))]
    (core/parallel-ranges!
      (quot (alength a) 3)
      (fn [^long start ^long end]
        (loop [i start]
          (when (< i end)
            (let [k (* amount (double (aget m i))) j (* 3 i)]
              (if (zero? k)
                (do (aset out j (aget a j)) (aset out (+ j 1) (aget a (+ j 1))) (aset out (+ j 2) (aget a (+ j 2))))
                (dotimes [c 3]
                  (let [x (double (aget a (+ j c)))]
                    (aset out (+ j c) (float (+ x (* k (- (double (aget b (+ j c))) x)))))))))
            (recur (inc i))))))
    (scene/image width height out)))

(defn- apply-layer [img layer opts]
  (if-let [{:keys [tone detail]} (and (layer-active? layer) (active-adjustments layer))]
    (let [^floats m (layer-mask img layer)
          adjusted (cond-> img
                     (seq tone)   (develop/tone tone)
                     (seq detail) (detail/local-contrast detail opts))]
      (blend img adjusted m (double (get layer :amount 1.0))))
    img))

(defn apply-local
  "Applies the layers of (:local settings) to a scene image, in order, and
  returns a new image (the same one when there is nothing to do)."
  [img settings & [opts]]
  (reduce (fn [im layer] (apply-layer im layer opts)) img (:local settings)))

;; ------------------------------------------------------- creating layers (UI)

(def type-labels
  {:linear "LINEAR GRADIENT" :radial "RADIAL GRADIENT" :brush "BRUSH" :range "RANGE"
   :subject "SUBJECT" :sky "SKY"})

(defn next-id
  "An id not used by any of `layers`."
  [layers]
  (inc (long (reduce max 0 (map :id layers)))))

(defn new-layer
  "A fresh layer of `type` with sensible starting geometry and no adjustments."
  [type id]
  {:id id :type type :visible true :amount 1.0 :invert false :feather 0.0 :adj {}
   :shape (case type
            :linear  {:x0 0.5 :y0 0.2 :x1 0.5 :y1 0.6}
            :radial  {:cx 0.5 :cy 0.5 :rx 0.3 :ry 0.25 :feather 0.5}
            :brush   {:strokes []}
            :subject {:rect [0.25 0.2 0.5 0.6]}
            {})
   :range (when (= type :range) {:luma {:lo 0.5 :hi 1.0 :smooth 0.1}})})

(defn layer-title
  "Short description for a list: its kind and the adjustments it makes."
  [layer]
  (let [adj (into {} (filter (fn [[_ v]] (and v (not (zero? (double v))))) (:adj layer)))]
    (str (type-labels (:type layer) "LAYER")
         (when (seq adj)
           (str " · " (clojure.string/join " " (map (fn [[k v]] (str (clojure.string/upper-case (name k)) " " (format "%+.2f" (double v)))) (take 2 adj))))))))

(defn- mix-channel
  "Channel value c blended toward t by k (0-1), rounded."
  ^long [^long c ^long t ^double k]
  (Math/round (+ (* (- 1.0 k) (double c)) (* k (double t)))))

(defn paint-mask
  "A copy of the packed-ARGB image with `mask` (a float plane, same size)
  painted over it in translucent red, for showing what a layer affects."
  [{:keys [width height pixels]} ^floats m]
  (let [^ints src pixels
        ^ints out (int-array (alength src))]
    (dotimes [i (alength src)]
      (let [p (aget src i) k (* 0.55 (double (aget m i)))
            r (bit-and (unsigned-bit-shift-right p 16) 0xFF)
            g (bit-and (unsigned-bit-shift-right p 8) 0xFF)
            b (bit-and p 0xFF)]
        (aset out i (unchecked-int (bit-or 0xFF000000
                                           (bit-shift-left (mix-channel r 255 k) 16)
                                           (bit-shift-left (mix-channel g 40 k) 8)
                                           (mix-channel b 40 k))))))
    {:width width :height height :pixels out}))
