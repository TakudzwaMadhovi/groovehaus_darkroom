(ns darkroom.imaging.pipeline
  "Maps a settings map (e.g. {:exposure 0.5 :denoise 40}) onto image stages.
  Every stage takes and returns a float scene image (darkroom.imaging.scene);
  convert the result with scene/->argb for display or export.

    1. denoise   (OpenCV non-local means; slow, so it runs first and is cached)
    2. color-nr  (chroma noise)
    3. geometry  (aspect crop, straighten, flip)
    4. dehaze
    5. tone      (exposure ... grain; see darkroom.imaging.develop)
    6. detail    (texture, clarity, sharpening; see darkroom.imaging.detail)

  Stages receive `opts`: :quality (:draft/:preview/:final) and :scale (preview
  width / source width, so pixel-sized radii look the same in a downscaled preview).

  Each stage owns a set of setting keys and is skipped when they are all at
  their defaults. To add a feature: add a stage (or keys to a stage) here and a
  control for it in the UI."
  (:require [darkroom.imaging.denoise :as denoise]
            [darkroom.imaging.detail :as detail]
            [darkroom.imaging.develop :as develop]
            [darkroom.imaging.geometry :as geometry]))

(def stages
  "Ordered stages. :op is (fn [image settings opts]); :quality? marks stages
  whose cost depends on opts {:quality :draft|:preview|:final}; :scaled? marks
  stages whose result depends on opts {:scale ...}."
  [{:id       :denoise
    :keys     [:denoise]
    :neutral? (fn [s] (zero? (long (get s :denoise 0))))
    :op       (fn [img s opts] (denoise/denoise img (get s :denoise 0) opts))
    :quality? true}
   {:id       :color-nr
    :keys     detail/color-nr-keys
    :neutral? detail/color-nr-neutral?
    :op       (fn [img s opts] (detail/reduce-color-noise img s opts))
    :scaled?  true}
   {:id       :geometry
    :keys     geometry/geometry-keys
    :neutral? geometry/geometry-neutral?
    :op       (fn [img s _] (geometry/geometry img s))}
   {:id       :dehaze
    :keys     detail/dehaze-keys
    :neutral? detail/dehaze-neutral?
    :op       (fn [img s _] (detail/dehaze img s))}
   {:id       :tone
    :keys     develop/tone-keys
    :neutral? develop/tone-neutral?
    :op       (fn [img s _] (develop/tone img s))}
   {:id       :detail
    :keys     detail/detail-keys
    :neutral? detail/detail-neutral?
    :op       (fn [img s opts] (detail/local-contrast img s opts))
    :scaled?  true}])

(def default-settings
  "Neutral value for every setting."
  (merge develop/defaults detail/defaults {:denoise 0}))

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
    (with-meta
      (fn render-cached
        ([settings] (render-cached settings nil))
        ([settings opts]
         (let [s    (full settings)
               prev @cache
               [img entries]
               (reduce (fn [[img acc valid?] [i {:keys [keys neutral? quality? scaled?] :as stage}]]
                         (let [k  (cond-> (select-keys s keys) scaled? (assoc ::scale (:scale opts)))
                               q  (if (and quality? (not (neutral? s))) (rank opts) 0)
                               [pk pimg pq] (get prev i)]
                           (if (and valid? pimg (= k pk) (>= (long pq) q))
                             [pimg (conj acc [k pimg pq]) true]
                             (let [out (run-stage img stage s opts)]
                               [out (conj acc [k out q]) false]))))
                       [source [] true]
                       (map-indexed vector stages))]
           (reset! cache entries)
           img)))
      {::cache cache})))

(defn cached-image
  "The image stage `id` (:geometry, :tone, ...) produced in the last render of
  `renderer`, or nil before the first render. For a stage that was skipped it is
  the image that passed through it."
  [renderer id]
  (when-let [cache (::cache (meta renderer))]
    (when-let [i (first (keep-indexed (fn [i st] (when (= id (:id st)) i)) stages))]
      (second (get @cache i)))))
