(ns darkroom.dev
  "Live-reload development loop. Start it with scripts/dev-mac.sh (or
  `lein run -m darkroom.dev`): the app opens, and whenever you save a file under
  src/ or resources/darkroom.css the changed namespaces (and everything that
  depends on them) are reloaded and the window is rebuilt in place. Application
  state, the open photo and the window position survive; nothing is rebuilt from
  GitHub or packaged.

  Only on the :dev profile; never part of the packaged app."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.namespace.dependency :as dep]
            [clojure.tools.namespace.file :as ns-file]
            [clojure.tools.namespace.parse :as parse]
            [darkroom.imaging.denoise :as denoise]
            [darkroom.ui.app :as app]
            [darkroom.ui.canvas :as canvas]
            [darkroom.ui.state :as st]
            [darkroom.ui.theme :as theme])
  (:import (java.io File)
           (javafx.application Platform)))

(set! *warn-on-reflection* false)

;; ------------------------------------------------------------------ pure core

(defn clj-files
  "All .clj files under `dir`."
  [dir]
  (filter #(and (.isFile ^File %) (str/ends-with? (.getName ^File %) ".clj"))
          (file-seq (io/file dir))))

(defn file->ns
  "{File ns-symbol} for the source files under `dir` that declare a namespace."
  [dir]
  (into {} (keep (fn [f] (when-let [decl (ns-file/read-file-ns-decl f)]
                           [f (parse/name-from-ns-decl decl)])))
        (clj-files dir)))

(defn dependency-graph
  "Namespace dependency info for the sources under `dir`:
  {:graph <tools.namespace graph, an edge per :require between project
   namespaces> :known <set of all project namespaces>}."
  [dir]
  (let [decls (keep ns-file/read-file-ns-decl (clj-files dir))
        known (set (map parse/name-from-ns-decl decls))]
    {:known known
     :graph (reduce (fn [g decl]
                      (let [n (parse/name-from-ns-decl decl)]
                        (reduce #(dep/depend %1 n %2) g (filter known (disj (parse/deps-from-ns-decl decl) n)))))
                    (dep/graph) decls)}))

(defn reload-order
  "`changed` namespaces plus everything that (transitively) requires them, with
  dependencies before dependents, so each namespace is reloaded after what it uses.
  `deps` is the result of `dependency-graph`; unknown namespaces are ignored."
  [{:keys [graph known]} changed]
  (let [changed  (filter known changed)
        affected (into (set changed) (mapcat #(dep/transitive-dependents graph %)) changed)]
    (vec (sort (dep/topo-comparator graph) affected))))

(defn changed-files
  "Files whose modification time differs from `seen` ({path millis})."
  [seen files]
  (filter #(not= (get seen (.getPath ^File %)) (.lastModified ^File %)) files))

;; ------------------------------------------------------------------ the loop

(def ^:private src-dir "src")
(def ^:private css-file (io/file "resources" "darkroom.css"))
(defonce ^:private watcher (atom nil))

(defn- say [& xs] (println (apply str "[dev] " xs)) (flush))

(defn- toast! [msg]
  (Platform/runLater #(st/toast! msg)))

(defn reload!
  "Reloads the namespaces for `files` (changed .clj files) and rebuilds the window.
  With no argument, reloads every namespace under src/. A failing reload leaves
  the previous code running, prints the error and shows a toast."
  ([] (reload! (clj-files src-dir)))
  ([files]
   (let [t0   (System/currentTimeMillis)
         f->n (file->ns src-dir)
         nss  (reload-order (dependency-graph src-dir) (keep f->n files))]
     (when (seq nss)
       (try
         (doseq [n nss] (require n :reload))
         (canvas/clear-cache!)
         (app/rebuild!)
         (say "reloaded " (str/join ", " (map #(str/replace (str %) "darkroom." "") nss))
              " (" (- (System/currentTimeMillis) t0) " ms)")
         true
         (catch Throwable t
           (say "RELOAD FAILED: " (.getMessage t))
           (toast! (str "RELOAD FAILED — " (str/upper-case (str (.getMessage t)))))
           false))))))

(defn reload-css!
  "Re-reads resources/darkroom.css and rebuilds the window."
  []
  (reset! theme/css-version (.lastModified css-file))
  (app/rebuild!)
  (say "stylesheet reloaded"))

(defn- poll-loop []
  (let [seen (atom (into {} (map (juxt #(.getPath ^File %) #(.lastModified ^File %))) (clj-files src-dir)))
        css  (atom (.lastModified css-file))]
    (fn []
      (loop []
        (try
          (let [files   (clj-files src-dir)
                changed (changed-files @seen files)]
            (when (seq changed)
              (swap! seen into (map (juxt #(.getPath ^File %) #(.lastModified ^File %))) files)
              (reload! changed))
            (when (not= @css (.lastModified css-file))
              (reset! css (.lastModified css-file))
              (reload-css!)))
          (catch Throwable t (say "watcher error: " (.getMessage t))))
        (Thread/sleep 400)
        (recur)))))

(defn start!
  "Opens the app (optionally on `file`) and starts watching for changes. Idempotent."
  ([] (start! nil))
  ([file]
   (when-not @watcher
     (doto (Thread. ^Runnable (poll-loop) "darkroom-dev-watch") (.setDaemon true) (.start))
     (reset! watcher true))
   (doto (Thread. ^Runnable denoise/warm-up! "darkroom-warmup") (.setDaemon true) (.start))
   (app/show! {:file file})
   (say "live reload on: save a file under src/ or resources/darkroom.css and the window updates")))

(defn -main [& [path]]
  (start! (when path (File. ^String (str path)))))
