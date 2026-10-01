(ns darkroom.plugin
  "Plugins: Clojure files in the app's `plugins` folder (next to the catalog),
  loaded when the editor starts. A plugin can

    (plugin/register-filter!
      {:id     :warm-glow                       ; keyword, unique
       :name   \"WARM GLOW\"                    ; shown on the PLUGINS tab
       :params [{:key :amount :label \"AMOUNT\" :min 0 :max 1 :step 0.01 :default 0.0}]
       :run    (fn [img params opts] img)})     ; params: {:amount 0.3}; returns a scene image

  - a filter appears on the Develop PLUGINS tab with a slider per parameter, runs
    after the built-in stages, is cached and undoable like any adjustment, and is
    skipped while all its parameters are at their defaults;
  - `img` is the editor's float scene image {:width :height :data} (linear, working
    colour space, interleaved RGB, values may exceed 1.0); `opts` carries :quality and
    :scale (preview size / full size, for pixel radii);
  - hooks: (plugin/on! :after-export (fn [{:keys [file]}] ...)) and
    (plugin/on! :after-import (fn [{:keys [paths]}] ...)).

  A plugin file is ordinary code with the app's full privileges: only put files
  there that you trust. Nothing is ever downloaded or loaded from anywhere else.
  Errors in a plugin are reported (see `errors`) and never stop the editor or
  the other plugins. Pure logic, no UI dependency."
  (:require [clojure.string :as str])
  (:import (java.io File)))

(defonce ^:private filters (atom {}))   ; id -> filter spec, in registration order via :order
(defonce ^:private hooks (atom {}))     ; event -> [fns]
(defonce ^:private load-errors (atom []))
(defonce ^:private order (atom 0))

(defn filters-list
  "Registered filters in registration order."
  []
  (vec (sort-by :order (vals @filters))))

(defn errors
  "[{:file name :message text}] for plugins that failed to load."
  []
  @load-errors)

(defn- check-filter! [{:keys [id name params run] :as spec}]
  (when-not (keyword? id) (throw (ex-info "A plugin filter needs a keyword :id" {:spec spec})))
  (when-not (and (string? name) (not (str/blank? name))) (throw (ex-info "A plugin filter needs a :name" {:id id})))
  (when-not (fn? run) (throw (ex-info "A plugin filter needs a :run function" {:id id})))
  (doseq [{:keys [key label min max default] :as p} params]
    (when-not (and (keyword? key) (string? label) (number? min) (number? max) (number? default) (< (double min) (double max))
                   (<= (double min) (double default) (double max)))
      (throw (ex-info "Each parameter needs :key :label :min :max and a :default inside the range" {:id id :param p})))))

(defn register-filter!
  "Registers an image filter (see the namespace docs). Registering the same :id
  again replaces the earlier filter. Returns the id."
  [spec]
  (check-filter! spec)
  (swap! filters assoc (:id spec)
         (assoc spec :order (or (get-in @filters [(:id spec) :order]) (swap! order inc))
                     :params (mapv #(assoc % :step (or (:step %) (/ (- (double (:max %)) (double (:min %))) 100.0))) (:params spec))))
  (:id spec))

(defn default-params
  "{param default} of filter `id`'s spec."
  [{:keys [params]}]
  (into {} (map (juxt :key :default) params)))

(defn settings-neutral?
  "True when no registered filter has a parameter away from its default in the
  frame settings' :plugins map ({filter-id {param value}}). Unknown filters
  (plugin removed) count as neutral."
  [settings]
  (every? (fn [[id vals]]
            (if-let [spec (@filters id)]
              (every? (fn [[k v]] (= (double v) (double (get (default-params spec) k v)))) vals)
              true))
          (:plugins settings)))

(defn apply-filters
  "Runs every registered filter whose parameters differ from their defaults on
  `img`, in registration order. A filter that throws is skipped (its error is
  printed once per message), so a faulty plugin can never break rendering."
  [img settings opts]
  (reduce (fn [im spec]
            (let [vals (merge (default-params spec) (get-in settings [:plugins (:id spec)]))]
              (if (= vals (default-params spec))
                im
                (try ((:run spec) im vals opts)
                     (catch Throwable t
                       (binding [*out* *err*] (println "plugin" (:id spec) "failed:" (.getMessage t)))
                       im)))))
          img (filters-list)))

;; ----------------------------------------------------------------- hooks

(defn on!
  "Calls `f` with an event map whenever `event` (:after-export or :after-import) happens."
  [event f]
  (swap! hooks update event (fnil conj []) f)
  nil)

(defn run-hooks!
  "Calls every function registered for `event` with `data`; failures are printed and skipped."
  [event data]
  (doseq [f (get @hooks event)]
    (try (f data)
         (catch Throwable t (binding [*out* *err*] (println "plugin hook" event "failed:" (.getMessage t)))))))

;; --------------------------------------------------------------- loading

(defn plugin-dir
  "The folder plugins are loaded from: `plugins` inside `data-dir`."
  ^File [^File data-dir]
  (File. data-dir "plugins"))

(defn- root-message
  "The message of the innermost cause (Clojure wraps errors from loaded code in
  compiler errors that hide what the plugin actually said)."
  [^Throwable t]
  (loop [t t] (if-let [c (.getCause t)] (recur c) (or (.getMessage t) (str (class t))))))

(defn load-dir!
  "Loads every .clj file directly inside `dir`, in name order. Returns the number
  loaded; failures are recorded in `errors`."
  [^File dir]
  (reset! load-errors [])
  (if-not (.isDirectory dir)
    0
    (count
      (for [^File f (sort-by #(.getName ^File %) (filter #(.endsWith (.getName ^File %) ".clj") (or (.listFiles dir) [])))
            :let [ok (try (binding [*ns* (create-ns 'user)] (load-file (.getPath f))) true
                          (catch Throwable t
                            (swap! load-errors conj {:file (.getName f) :message (root-message t)})
                            false))]
            :when ok]
        f))))

(defn reset-for-tests!
  "Forgets every filter, hook and error (tests only)."
  []
  (reset! filters {}) (reset! hooks {}) (reset! load-errors []) (reset! order 0))
