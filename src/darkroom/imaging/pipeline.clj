(ns darkroom.imaging.pipeline
  "Maps a settings map (e.g. {:exposure 0.5 :denoise 40}) onto image stages:

    1. denoise   (OpenCV non-local means; slow, so it runs first and is cached)
    2. geometry  (aspect crop, straighten, flip)
    3. tone      (exposure ... grain; see darkroom.imaging.develop)

  Each stage owns a set of setting keys and is skipped when they are all at
  their defaults. To add a feature: add a stage (or keys to a stage) here and a
  control for it in the UI."
  (:require [darkroom.imaging.denoise :as denoise]
            [darkroom.imaging.develop :as develop]))

(def stages
  "Ordered stages. :op is (fn [image settings opts]); :quality? marks stages
  whose cost depends on opts {:quality :draft|:preview|:final}."
  [{:id       :denoise
    :keys     [:denoise]
    :neutral? (fn [s] (zero? (long (get s :denoise 0))))
    :op       (fn [img s opts] (denoise/denoise img (get s :denoise 0) opts))
    :quality? true}
   {:id       :geometry
    :keys     develop/geometry-keys
    :neutral? develop/geometry-neutral?
    :op       (fn [img s _] (develop/geometry img s))}
   {:id       :tone
    :keys     develop/tone-keys
    :neutral? develop/tone-neutral?
    :op       (fn [img s _] (develop/tone img s))}])

(def default-settings
  "Neutral value for every setting."
  (assoc develop/defaults :denoise 0))

(defn- full [settings] (merge default-settings settings))

(defn- run-stage [img {:keys [neutral? op]} settings opts]
  (if (neutral? settings) img (op img settings opts)))

(defn render
  "Applies every non-neutral stage to `source` and returns the result.
  `opts` {:quality :final} asks stages for full quality (use for export)."
  ([source settings] (render source settings nil))
  ([source settings opts]
   (let [s (full settings)]
     (reduce #(run-stage %1 %2 s opts) source stages))))

(def ^:private quality-rank {:draft 0 :preview 1 :final 2})

(defn- rank [opts] (quality-rank (or (:quality opts) :preview) 1))

(defn renderer
  "Returns (fn [settings] / [settings opts]) -> image for interactive use on one
  `source`. It remembers each stage's last result and re-runs only from the
  first stage whose settings changed, so dragging a tone slider never re-runs
  the slow denoise. A cached result at the same or higher quality satisfies a
  request for lower quality. Not for concurrent calls; the UI calls it from a
  single render thread."
  [source]
  (let [cache (atom [])]
    (fn render-cached
      ([settings] (render-cached settings nil))
      ([settings opts]
       (let [s    (full settings)
             prev @cache
             [img entries]
             (reduce (fn [[img acc valid?] [i {:keys [keys neutral? quality?] :as stage}]]
                       (let [k  (select-keys s keys)
                             q  (if (and quality? (not (neutral? s))) (rank opts) 0)
                             [pk pimg pq] (get prev i)]
                         (if (and valid? pimg (= k pk) (>= (long pq) q))
                           [pimg (conj acc [k pimg pq]) true]
                           (let [out (run-stage img stage s opts)]
                             [out (conj acc [k out q]) false]))))
                     [source [] true]
                     (map-indexed vector stages))]
         (reset! cache entries)
         img)))))
