(ns darkroom.ui.state
  "Application state and the actions that change it. All functions run on the
  JavaFX thread. Views watch `state` and redraw the parts that changed.

  State keys:
    :catalog   darkroom.catalog data (shoots, frames, adjustments, history)
    :view      :library | :develop
    :shoot     current shoot id
    :cur       path of the current frame (string) or nil
    :tab       develop tab (:basic :curve :look :crop :presets :history)
    :lf        library filter (:all :picks :edited)
    :tsz       library thumbnail size 120-360
    :before    true while showing the unedited image
    :exporting export overlay open?
    :fmt :size :q :export-dir   export options
    :adding / :toast            transient UI"
  (:require [clojure.java.io :as io]
            [darkroom.catalog :as cat]
            [darkroom.imaging.browser :as browser])
  (:import (java.io File)
           (java.util.concurrent Executors ScheduledExecutorService ScheduledFuture ThreadFactory TimeUnit)))

(defonce state
  (atom {:catalog cat/empty-catalog :view :library :shoot nil :cur nil :tab :basic
         :lf :all :tsz 220 :before false :exporting false :fmt :jpeg :size 2048 :q 90
         :export-dir nil :adding false :toast nil}))

;; ------------------------------------------------------------- derivations

(defn frames
  "Paths in the current shoot."
  ([] (frames @state))
  ([st] (vec (:paths (cat/shoot (:catalog st) (:shoot st))))))

(defn cur-path [st] (:cur st))

(defn cur-adj [st] (when-let [p (:cur st)] (cat/adj (:catalog st) p)))

(defn visible-frames
  "Frames shown in the library after the filter."
  [st]
  (let [c (:catalog st) fs (frames st)]
    (case (:lf st)
      :picks  (filterv #(= 5 (cat/rating c %)) fs)
      :edited (filterv #(cat/edited? c %) fs)
      fs)))

;; ----------------------------------------------------------------- persist

(defonce ^:private ^ScheduledExecutorService saver
  (Executors/newSingleThreadScheduledExecutor
    (reify ThreadFactory (newThread [_ r] (doto (Thread. ^Runnable r "darkroom-save") (.setDaemon true))))))

(defonce ^:private pending-save (atom nil))
(defonce catalog-file (atom (cat/default-file)))

(defn save-soon!
  "Writes the catalog about 600 ms after the last change (debounced)."
  []
  (when-let [^ScheduledFuture f @pending-save] (.cancel f false))
  (let [c (:catalog @state)]
    (reset! pending-save
            (.schedule saver
                       ^Runnable (fn []
                                   (try (cat/save! c @catalog-file)
                                        (catch Throwable t
                                          (binding [*out* *err*] (println "catalog save failed:" (.getMessage t))))))
                       600 TimeUnit/MILLISECONDS))))

(defn save-now!
  "Flushes the catalog synchronously (use on exit)."
  []
  (when-let [^ScheduledFuture f @pending-save] (.cancel f false))
  (try (cat/save! (:catalog @state) @catalog-file) (catch Throwable _ nil)))

(defn- update-catalog! [f & args]
  (swap! state update :catalog #(apply f % args))
  (save-soon!))

;; ----------------------------------------------------------------- actions

(defn toast!
  "Shows a transient message."
  [msg]
  (swap! state assoc :toast {:msg msg :id (System/nanoTime)}))

(defn go!
  "Switch view. Opening Develop with a frame outside the current shoot jumps to
  the shoot's first frame; an empty shoot stays in Library."
  ([view] (go! view nil))
  ([view path]
   (swap! state
          (fn [st]
            (let [fs (frames st)
                  c  (or path (:cur st))
                  c  (if (and (= view :develop) (not (some #{c} fs))) (first fs) c)]
              (if (and (= view :develop) (nil? c))
                st
                (assoc st :view view :cur c)))))))

(defn select! [path] (swap! state assoc :cur path))

(defn nav!
  "Previous/next frame within the shoot, wrapping."
  [d]
  (swap! state
          (fn [st]
            (let [fs (frames st)]
              (if (empty? fs)
                st
                (let [k (max 0 (.indexOf ^java.util.List fs (:cur st)))]
                  (assoc st :cur (fs (mod (+ k d) (count fs))))))))))

(defn move!
  "Library keyboard navigation: moves the selection `n` frames through the
  *visible* (filtered) frames, stopping at the ends. Frames are laid out in a
  grid, so Up/Down pass the column count."
  [n]
  (swap! state
         (fn [st]
           (let [vis (visible-frames st)]
             (if (empty? vis)
               st
               (let [k (.indexOf ^java.util.List vis (:cur st))
                     k (if (neg? k) 0 (max 0 (min (dec (count vis)) (+ k n))))]
                 (assoc st :cur (vis k))))))))

(defn select-shoot! [id]
  (swap! state
         (fn [st]
           (let [fs (vec (:paths (cat/shoot (:catalog st) id)))]
             (assoc st :shoot id :lf :all :cur (or (first fs) (:cur st)))))))

(defn create-shoot!
  "Creates a (possibly empty) shoot and makes it current. Returns its id."
  [name paths]
  (let [[c id] (cat/add-shoot (:catalog @state) name paths)]
    (when id
      (swap! state assoc :catalog c :shoot id :adding false :lf :all
             :cur (first paths))
      (save-soon!))
    id))

(defn image-paths
  "Expands files/folders into image file paths (folders are scanned one level)."
  [files]
  (vec (mapcat (fn [^File f]
                 (cond (.isDirectory f) (map #(.getPath ^File %) (browser/scan f))
                       (browser/supported-image? f) [(.getPath f)]
                       :else []))
               files)))

(defn import-paths!
  "Adds images to the current shoot (creating FIRST SHOOT if there is none).
  Selects the first newly added frame. Returns the number of new frames."
  [paths]
  (let [paths (vec (distinct paths))]
    (when (seq paths)
      (when-not (:shoot @state) (create-shoot! "FIRST SHOOT" []))
      (let [id     (:shoot @state)
            before (set (frames))]
        (update-catalog! cat/add-paths id paths)
        (let [new (vec (remove before paths))]
          (when (seq new) (swap! state assoc :cur (first new) :view :library))
          (count new))))))

(defn rate!
  "Sets the rating of the current frame (same value again clears it)."
  [n]
  (when-let [p (:cur @state)] (update-catalog! cat/set-rating p n)))

(defn toggle-pick! [path] (update-catalog! cat/toggle-pick path))

(defn set-adj!
  "Live adjustment of the current frame (no history entry yet)."
  [k v]
  (when-let [p (:cur @state)] (swap! state update :catalog cat/set-adj p k v)))

(defn commit! [label]
  (when-let [p (:cur @state)] (update-catalog! cat/commit p label)))

(defn reset-adj! [k label default]
  (set-adj! k default)
  (commit! (str label " RESET")))

(defn apply-preset! [preset-name]
  (when-let [p (:cur @state)] (update-catalog! cat/apply-preset p preset-name)))

(defn revert! [i]
  (when-let [p (:cur @state)] (update-catalog! cat/revert-to p i)))

;; ----------------------------------------------------------------- startup

(defn load-catalog!
  "Loads the saved catalog and picks a starting shoot/frame."
  []
  (let [c (cat/load! @catalog-file)
        s (first (:shoots c))]
    (swap! state assoc :catalog c :shoot (:id s) :cur (first (:paths s)))))

(defn open-file!
  "Makes sure `file` is in a shoot named after its folder, selects it and opens
  Develop (used for command-line arguments)."
  [^File file]
  (let [file (.getAbsoluteFile file)
        path (.getPath file)
        dir  (.getParentFile file)
        c    (:catalog @state)
        existing (first (filter #(some #{path} (:paths %)) (:shoots c)))]
    (if existing
      (swap! state assoc :shoot (:id existing) :cur path)
      (do (create-shoot! (.getName dir) (mapv #(.getPath ^File %) (browser/scan dir)))
          (swap! state assoc :cur path)))
    (go! :develop path)))
