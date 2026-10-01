(ns darkroom.imaging.pipeline
  "Maps a settings map (e.g. {:brightness 20}) onto image operations.

  To add a feature: write an `(fn [image value])` in darkroom.imaging.core (or
  a new namespace), register it in `operations` with a neutral value, and add a
  control for it in the UI. Operations run in the order listed; denoise comes
  first so later tone changes do not amplify noise."
  (:require [darkroom.imaging.core :as core]
            [darkroom.imaging.denoise :as denoise]))

(def operations
  "Ordered stages. :op is (fn [image value]) or, with :opts? true,
  (fn [image value opts]) where opts is the map given to `render`
  (currently {:quality :draft|:preview|:final})."
  [{:key :denoise    :op denoise/denoise        :neutral 0   :opts? true}
   {:key :brightness :op core/adjust-brightness :neutral 0}
   {:key :contrast   :op core/adjust-contrast   :neutral 0}
   {:key :gamma      :op core/adjust-gamma      :neutral 1.0}
   {:key :saturation :op core/adjust-saturation :neutral 0}])

(def default-settings
  (into {} (map (juxt :key :neutral)) operations))

(defn- run-stage [img {:keys [key op neutral opts?]} settings opts]
  (let [v (get settings key neutral)]
    (if (== v neutral)
      img
      (if opts? (op img v opts) (op img v)))))

(defn render
  "Applies every non-neutral setting to `source` and returns the result.
  `opts` {:quality :final} asks stages for full quality (use for export)."
  ([source settings] (render source settings nil))
  ([source settings opts]
   (reduce #(run-stage %1 %2 settings opts) source operations)))

(def ^:private quality-rank {:draft 0 :preview 1 :final 2})

(defn- rank [opts] (quality-rank (or (:quality opts) :preview) 1))

(defn renderer
  "Returns (fn [settings] / [settings opts]) -> image for interactive use on one
  `source`. It remembers each stage's last result and re-runs only from the
  first stage whose setting changed, so dragging brightness never re-runs the
  slow denoise. A cached result at the same or higher quality satisfies a
  request for lower quality. Not for concurrent calls; the UI calls it from a
  single render thread."
  [source]
  (let [cache (atom [])]
    (fn render-cached
      ([settings] (render-cached settings nil))
      ([settings opts]
       (let [prev @cache
             [img stages]
             (reduce (fn [[img acc valid?] [i {:keys [key neutral opts?] :as stage}]]
                       (let [v  (get settings key neutral)
                             q  (if (and opts? (not (== v neutral))) (rank opts) 0)
                             [pv pimg pq] (get prev i)]
                         (if (and valid? pimg (== v pv) (>= (long pq) q))
                           [pimg (conj acc [v pimg pq]) true]
                           (let [out (run-stage img stage settings opts)]
                             [out (conj acc [v out q]) false]))))
                     [source [] true]
                     (map-indexed vector operations))]
         (reset! cache stages)
         img)))))
