(ns groovehaus.looks.registry
  "Look registry and look-config validation.

  Look config (what the UI/sidecar stores):
    {:look-id :vintage-bw
     :params {:intensity 0.8 :grain-amount 0.2 :vignette-radius 0.6 :fade 0.1}
     :blend-mode :overlay      ; optional, defaults to the look's own default
     :enabled true}            ; optional, defaults to true

  Param types: :unit (0..1), :number (:min/:max), :int, :color ([r g b] 0..1 or \"#rrggbb\"),
  :enum (:values). Numbers outside range are clamped (slider friendly); wrong types and
  unknown keys throw ex-info."
  (:require [groovehaus.looks.blend :as blend]))

(defonce ^:private registry (atom {}))

(def common-params {:intensity {:type :unit :default 1.0}})

(defn register-look!
  "look: {:id kw :name str :params {k spec} :blend-mode kw :render (fn [buf params])}"
  [{:keys [id] :as look}]
  (swap! registry assoc id (update look :blend-mode #(or % :normal)))
  id)

(defn get-look [id]
  (or (get @registry id)
      (throw (ex-info (str "Unknown look: " id) {:look-id id :known (sort (keys @registry))}))))

(defn looks [] (vals @registry))

(defn- bad! [k msg data] (throw (ex-info (str "Invalid param " k ": " msg) (assoc data :param k))))

(defn- clamp [lo hi ^double v] (Math/max (double lo) (Math/min (double hi) v)))

(defn- parse-hex [k s]
  (if-let [[_ r g b] (re-matches #"#?([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})" s)]
    (mapv #(/ (Integer/parseInt % 16) 255.0) [r g b])
    (bad! k "expected \"#rrggbb\"" {:value s})))

(defn- coerce [k {:keys [type min max values]} v]
  (case type
    :unit (if (number? v) (clamp 0.0 1.0 (double v)) (bad! k "expected number" {:value v}))
    :number (if (number? v) (clamp min max (double v)) (bad! k "expected number" {:value v}))
    :int (if (number? v) (long v) (bad! k "expected integer" {:value v}))
    :color (let [c (if (string? v) (parse-hex k v) v)]
             (if (and (sequential? c) (= 3 (count c)) (every? number? c))
               (mapv #(clamp 0.0 1.0 (double %)) c)
               (bad! k "expected [r g b] or \"#rrggbb\"" {:value v})))
    :enum (if (contains? values v) v (bad! k (str "expected one of " (sort values)) {:value v}))))

(defn param-specs [look] (merge common-params (:params look)))

(defn normalize-config
  "Validates a look config and returns it fully populated: every param present, typed and
  clamped. Equal inputs yield equal outputs, so the result is a safe cache key."
  [{:keys [look-id params blend-mode enabled] :or {enabled true}}]
  (let [look (get-look look-id)
        specs (param-specs look)
        unknown (remove specs (keys params))
        mode (or blend-mode (:blend-mode look))]
    (when (seq unknown)
      (throw (ex-info (str "Unknown params for " look-id ": " (vec unknown))
                      {:look-id look-id :unknown (vec unknown) :allowed (sort (keys specs))})))
    (when-not (blend/modes mode)
      (throw (ex-info (str "Unknown blend mode: " mode) {:blend-mode mode :allowed (sort blend/modes)})))
    {:look-id look-id
     :params (reduce-kv (fn [m k spec] (assoc m k (coerce k spec (get params k (:default spec)))))
                        {} specs)
     :blend-mode mode
     :enabled (boolean enabled)}))
