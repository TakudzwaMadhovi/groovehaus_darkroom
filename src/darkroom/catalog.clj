(ns darkroom.catalog
  "The library: shoots (named lists of image files) and per-frame edit state
  (rating, adjustments, history). Pure functions over plain data, plus EDN
  load/save. Edits are non-destructive: only paths and adjustment values are
  stored, never pixels.

  Catalog shape:
    {:shoots [{:id \"s1\" :name \"FIRST SHOOT\" :paths [\"/abs/a.jpg\" ...]}]
     :frames {\"/abs/a.jpg\" {:rating 0-5 :adj {...} :history [{:label :adj}]}}}"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [darkroom.imaging.pipeline :as pipeline])
  (:import (java.io File)
           (java.nio.file Files StandardCopyOption)))

(def history-limit 40)

(def empty-catalog {:shoots [] :frames {}})

(def presets
  "[name adjustments] from the design handoff. Applying one resets every
  adjustment except angle, aspect and flip."
  [["ON THE DECKS" {:bw 1.0 :contrast 0.35 :grain 0.5 :vignette 0.4 :fade 0.05 :exposure 0.1 :shadows -0.2}]
   ["NEON EDGE"    {:contrast 0.3 :saturation 0.35 :shadows -0.3 :vignette 0.3 :highlights -0.1}]
   ["AFTER HOURS"  {:exposure -0.35 :contrast 0.2 :temp -0.2 :saturation -0.2 :grain 0.3 :vignette 0.5}]
   ["SUNLIT"       {:temp 0.45 :exposure 0.2 :fade 0.2 :saturation 0.1 :highlights -0.2}]
   ["SILVER"       {:bw 1.0 :contrast -0.1 :fade 0.3 :grain 0.25 :highlights -0.2 :shadows 0.2}]
   ["ORIGINAL"     {}]])

;; ------------------------------------------------------------------ naming

(defn frame-name
  "Display name for a file: base name without extension, upper-cased, 14 chars."
  [path]
  (let [n (.getName (File. (str path)))
        base (str/replace n #"\.[^.]+$" "")]
    (str/upper-case (subs base 0 (min 14 (count base))))))

(defn shoot-name
  "Upper-cased, trimmed, at most 28 characters; nil if blank."
  [s]
  (let [n (str/upper-case (str/trim (str s)))]
    (when-not (str/blank? n) (subs n 0 (min 28 (count n))))))

;; ------------------------------------------------------------------ shoots

(defn- next-id [catalog]
  (let [taken (set (map :id (:shoots catalog)))]
    (first (remove taken (map #(str "s" %) (iterate inc (inc (count taken))))))))

(defn add-shoot
  "Adds a shoot named `name` holding `paths`. Returns [catalog id], or
  [catalog nil] when the name is blank."
  [catalog name paths]
  (if-let [n (shoot-name name)]
    (let [id (next-id catalog)]
      [(update catalog :shoots conj {:id id :name n :paths (vec (distinct (map str paths)))}) id])
    [catalog nil]))

(defn add-paths
  "Appends `paths` to a shoot, skipping ones already in it."
  [catalog shoot-id paths]
  (update catalog :shoots
          (fn [shoots]
            (mapv (fn [s]
                    (if (= shoot-id (:id s))
                      (update s :paths (fn [old] (let [seen (set old)]
                                                   (into old (remove seen) (distinct (map str paths))))))
                      s))
                  shoots))))

(defn shoot [catalog id] (first (filter #(= id (:id %)) (:shoots catalog))))

;; ------------------------------------------------------------------ frames

(defn fresh-frame []
  {:rating 0
   :adj pipeline/default-settings
   :history [{:label "IMPORTED" :adj pipeline/default-settings}]})

(defn frame
  "Edit state for `path` (a fresh, unedited frame if never touched)."
  [catalog path]
  (get-in catalog [:frames (str path)] (fresh-frame)))

(defn adj [catalog path] (merge pipeline/default-settings (:adj (frame catalog path))))

(defn edited? [catalog path] (not= (adj catalog path) pipeline/default-settings))

(defn rating [catalog path] (:rating (frame catalog path)))

(defn picks [catalog paths] (filter #(= 5 (rating catalog %)) paths))

(defn- update-frame [catalog path f]
  (assoc-in catalog [:frames (str path)] (f (frame catalog path))))

(defn set-adj
  "Sets one adjustment (live; no history entry until `commit`)."
  [catalog path k v]
  (update-frame catalog path #(assoc-in % [:adj k] v)))

(defn commit
  "Records the current adjustments in the history under `label`, unless they
  equal the latest entry. Keeps the last `history-limit` entries plus the new one."
  [catalog path label]
  (update-frame catalog path
                (fn [f]
                  (let [cur (merge pipeline/default-settings (:adj f))
                        h   (:history f)]
                    (if (= cur (:adj (peek h)))
                      f
                      (assoc f :history (conj (vec (take-last history-limit h)) {:label label :adj cur})))))))

(defn set-rating
  "Sets the rating to `n` (0-5); choosing the current rating again clears it."
  [catalog path n]
  (update-frame catalog path #(assoc % :rating (if (= n (:rating %)) 0 n))))

(defn toggle-pick
  "Star toggle: rating 5 <-> 0."
  [catalog path]
  (update-frame catalog path #(assoc % :rating (if (= 5 (:rating %)) 0 5))))

(defn apply-preset
  "Replaces every adjustment with the preset's (defaults elsewhere), keeping the
  frame's angle, aspect and flip, and records a history entry."
  [catalog path preset-name]
  (let [o (some (fn [[n o]] (when (= n preset-name) o)) presets)
        f (frame catalog path)
        keep (select-keys (merge pipeline/default-settings (:adj f)) [:angle :aspect :flip])
        new-adj (merge pipeline/default-settings o keep)]
    (-> catalog
        (update-frame path #(assoc % :adj new-adj))
        (commit path preset-name))))

(defn revert-to
  "Restores the adjustments of history entry `i` (no new history entry)."
  [catalog path i]
  (if-let [entry (get (:history (frame catalog path)) i)]
    (update-frame catalog path #(assoc % :adj (merge pipeline/default-settings (:adj entry))))
    catalog))

;; ------------------------------------------------------------ persistence

(defn app-dir
  "Per-user data directory for the catalog."
  ^File []
  (let [os   (.toLowerCase (System/getProperty "os.name" ""))
        home (System/getProperty "user.home")]
    (cond
      (str/includes? os "mac") (File. home "Library/Application Support/Groovehaus Darkroom")
      (str/includes? os "win") (File. (or (System/getenv "APPDATA") home) "Groovehaus Darkroom")
      :else                    (File. home ".local/share/groovehaus-darkroom"))))

(defn default-file ^File [] (File. (app-dir) "catalog.edn"))

(defn save!
  "Writes the catalog as EDN atomically (temp file + move)."
  [catalog ^File file]
  (let [dir (.getParentFile file)]
    (.mkdirs dir)
    (let [tmp (File/createTempFile ".catalog-" ".tmp" dir)]
      (try
        (spit tmp (binding [*print-length* nil *print-level* nil] (pr-str catalog)))
        (Files/move (.toPath tmp) (.toPath file)
                    (into-array java.nio.file.CopyOption [StandardCopyOption/REPLACE_EXISTING]))
        (finally (Files/deleteIfExists (.toPath tmp)))))))

(defn load!
  "Reads a catalog; a missing or unreadable file yields the empty catalog
  (an unreadable one is first copied aside as catalog.edn.bad)."
  [^File file]
  (if-not (.exists file)
    empty-catalog
    (try
      (let [c (edn/read-string (slurp file))]
        (if (and (map? c) (vector? (:shoots c)) (map? (:frames c))) c empty-catalog))
      (catch Exception _
        (try (Files/copy (.toPath file) (.toPath (File. (str file ".bad")))
                         (into-array java.nio.file.CopyOption [StandardCopyOption/REPLACE_EXISTING]))
             (catch Exception _ nil))
        empty-catalog))))
