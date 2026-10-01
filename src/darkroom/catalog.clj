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
            [darkroom.imaging.geometry :as geometry]
            [darkroom.imaging.paths :as paths]
            [darkroom.imaging.pipeline :as pipeline])
  (:import (java.io File)
           (java.nio.file Files StandardCopyOption)))

(def history-limit 40)

(def empty-catalog {:shoots [] :frames {}})

(def preserved-keys
  "Settings a preset or a paste of settings never touches: where and what the
  picture is (geometry), its retouching (spots) and its masks."
  (into (vec geometry/geometry-keys) [:spots :local]))

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
  "Display name for a frame: base name without extension, upper-cased, 14
  characters (a virtual copy is shortened to fit ' VCn')."
  [path]
  (let [n (.getName (File. (paths/source-file path)))
        base (str/replace n #"\.[^.]+$" "")
        copy (paths/copy-number path)]
    (if copy
      (str (str/upper-case (subs base 0 (min 10 (count base)))) " VC" copy)
      (str/upper-case (subs base 0 (min 14 (count base)))))))

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
   :history [{:label "IMPORTED" :adj pipeline/default-settings}]
   :hpos 0})

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

(defn- hpos
  "Index of the history entry the frame is at (the last one for catalogs saved
  before undo/redo existed)."
  [f]
  (or (:hpos f) (dec (count (:history f)))))

(defn commit
  "Records the current adjustments in the history under `label`, unless they
  equal the entry the frame is at. Anything after that entry (steps that were
  undone) is dropped, and the history keeps the last `history-limit` entries
  plus the new one."
  [catalog path label]
  (update-frame catalog path
                (fn [f]
                  (let [cur (merge pipeline/default-settings (:adj f))
                        h   (:history f)
                        i   (hpos f)]
                    (if (= cur (:adj (get h i)))
                      f
                      (let [h' (conj (vec (take-last history-limit (subvec h 0 (min (count h) (inc i))))) {:label label :adj cur})]
                        (assoc f :history h' :hpos (dec (count h')))))))))

(defn set-rating
  "Sets the rating to `n` (0-5); choosing the current rating again clears it."
  [catalog path n]
  (update-frame catalog path #(assoc % :rating (if (= n (:rating %)) 0 n))))

(defn assign-rating
  "Sets the rating to exactly `n` (0-5); unlike set-rating the same value does
  not toggle, so several frames can be given one rating."
  [catalog path n]
  (update-frame catalog path #(assoc % :rating (max 0 (min 5 (long n))))))

(defn toggle-pick
  "Star toggle: rating 5 <-> 0."
  [catalog path]
  (update-frame catalog path #(assoc % :rating (if (= 5 (:rating %)) 0 5))))

(defn apply-preset
  "Replaces every adjustment with the preset's (defaults elsewhere), keeping the
  frame's geometry (crop, turns, flips, perspective, lens), its spots and its
  local-adjustment layers, and records a history entry."
  [catalog path preset-name]
  (let [o (some (fn [[n o]] (when (= n preset-name) o)) presets)
        f (frame catalog path)
        keep (select-keys (merge pipeline/default-settings (:adj f)) preserved-keys)
        new-adj (merge pipeline/default-settings o keep)]
    (-> catalog
        (update-frame path #(assoc % :adj new-adj))
        (commit path preset-name))))

(defn revert-to
  "Moves the frame to history entry `i`, restoring its adjustments (the later
  entries stay, so the move can be undone with `redo`, until a new edit)."
  [catalog path i]
  (if-let [entry (get (:history (frame catalog path)) i)]
    (update-frame catalog path #(assoc % :adj (merge pipeline/default-settings (:adj entry)) :hpos i))
    catalog))

(defn history-pos
  "Index of the history entry frame `path` is at."
  [catalog path]
  (hpos (frame catalog path)))

(defn can-undo? [catalog path] (pos? (hpos (frame catalog path))))

(defn can-redo? [catalog path]
  (let [f (frame catalog path)] (< (hpos f) (dec (count (:history f))))))

(defn undo
  "One step back in the history. Edits made since the last commit count as the
  step to undo: they are discarded first, returning to the committed state.
  Unchanged at the start."
  [catalog path]
  (let [f (frame catalog path) i (hpos f)
        live? (not= (merge pipeline/default-settings (:adj f)) (merge pipeline/default-settings (:adj (get (:history f) i))))]
    (cond live? (revert-to catalog path i)
          (pos? i) (revert-to catalog path (dec i))
          :else catalog)))

(defn redo
  "One step forward again (or unchanged at the end)."
  [catalog path]
  (let [f (frame catalog path) i (hpos f)]
    (if (< i (dec (count (:history f)))) (revert-to catalog path (inc i)) catalog)))

;; ------------------------------------------------------ flags, labels, notes

(def colour-labels [:red :yellow :green :blue :purple])

(defn rejected? [catalog path] (boolean (:reject (frame catalog path))))

(defn toggle-reject
  "Flags the frame as rejected, or clears the flag."
  [catalog path]
  (update-frame catalog path #(assoc % :reject (not (:reject %)))))

(defn colour [catalog path] (:colour (frame catalog path)))

(defn set-colour
  "Sets the colour label; the same colour again (or nil) clears it."
  [catalog path c]
  (update-frame catalog path #(assoc % :colour (when (and c (not= c (:colour %))) c))))

(defn parse-keywords
  "Keywords from free text: split on commas, trimmed, lower-cased, no blanks or duplicates."
  [text]
  (vec (distinct (remove str/blank? (map #(str/lower-case (str/trim %)) (str/split (str text) #","))))))

(defn keywords [catalog path] (vec (:keywords (frame catalog path))))

(defn add-keywords
  "Adds the keywords in `text` (comma separated) to the frame."
  [catalog path text]
  (update-frame catalog path #(assoc % :keywords (vec (sort (distinct (concat (:keywords %) (parse-keywords text))))))))

(defn remove-keyword [catalog path kw]
  (update-frame catalog path #(assoc % :keywords (vec (remove #{(str/lower-case (str kw))} (:keywords %))))))

(def meta-fields [:title :caption :creator :copyright])

(defn frame-meta
  "{:title :caption :creator :copyright} of the frame (only the ones set)."
  [catalog path]
  (select-keys (:meta (frame catalog path)) meta-fields))

(defn set-meta
  "Sets one descriptive field (:title :caption :creator :copyright); blank clears it."
  [catalog path k v]
  (assert (some #{k} meta-fields))
  (update-frame catalog path
                (fn [f] (if (str/blank? (str v)) (update f :meta dissoc k) (assoc-in f [:meta k] (str/trim (str v)))))))

;; ----------------------------------------------------------------- snapshots

(defn snapshots [catalog path] (vec (:snapshots (frame catalog path))))

(defn add-snapshot
  "Stores the frame's current adjustments under `name`."
  [catalog path name]
  (update-frame catalog path
                (fn [f] (update f :snapshots (fnil conj [])
                                {:name (str/upper-case (subs (str/trim (str name)) 0 (min 28 (count (str/trim (str name))))))
                                 :adj (merge pipeline/default-settings (:adj f))}))))

(defn apply-snapshot
  "Restores snapshot `i`, as a new history step."
  [catalog path i]
  (if-let [{:keys [name adj]} (get (snapshots catalog path) i)]
    (-> catalog
        (update-frame path #(assoc % :adj (merge pipeline/default-settings adj)))
        (commit path (str "SNAPSHOT " name)))
    catalog))

(defn delete-snapshot [catalog path i]
  (update-frame catalog path (fn [f] (update f :snapshots (fn [v] (vec (concat (take i v) (drop (inc i) v))))))))

;; ------------------------------------------------------------- user presets

(defn user-presets
  "[[name adjustments]] saved by the user, in the order saved."
  [catalog]
  (mapv (juxt :name :adj) (:user-presets catalog)))

(defn save-preset
  "Saves the frame's adjustments (without its geometry, spots and masks, which
  belong to the picture) as preset `name`, replacing one of that name."
  [catalog path name]
  (if-let [n (shoot-name name)]
    (let [adj (apply dissoc (adj catalog path) preserved-keys)
          others (remove #(= n (:name %)) (:user-presets catalog))]
      (assoc catalog :user-presets (vec (concat others [{:name n :adj adj}]))))
    catalog))

(defn delete-preset [catalog name]
  (update catalog :user-presets (fn [ps] (vec (remove #(= name (:name %)) ps)))))

(defn apply-user-preset
  "Applies a saved preset like a built-in one: everything but the frame's
  geometry, spots and masks is replaced by the preset's values."
  [catalog path name]
  (if-let [{o :adj} (first (filter #(= name (:name %)) (:user-presets catalog)))]
    (let [keep (select-keys (adj catalog path) preserved-keys)]
      (-> catalog
          (update-frame path #(assoc % :adj (merge pipeline/default-settings (apply dissoc o preserved-keys) keep)))
          (commit path name)))
    catalog))

(defn presets->edn
  "The user's presets as text, to share or back up."
  [catalog]
  (binding [*print-length* nil *print-level* nil]
    (pr-str {:groovehaus-presets 1 :presets (vec (:user-presets catalog))})))

(defn import-presets
  "Adds the presets in `text` (from presets->edn). Returns [catalog n-imported];
  malformed text imports nothing."
  [catalog text]
  (try
    (let [{:keys [presets groovehaus-presets]} (edn/read-string text)
          good (filter #(and (map? %) (string? (:name %)) (map? (:adj %)) (not (str/blank? (:name %)))) presets)]
      (if (and (= 1 groovehaus-presets) (seq good))
        (let [good (mapv (fn [p] {:name (shoot-name (:name p)) :adj (apply dissoc (:adj p) preserved-keys)}) good)
              names (set (map :name good))
              others (remove #(names (:name %)) (:user-presets catalog))]
          [(assoc catalog :user-presets (vec (concat others good))) (count good)])
        [catalog 0]))
    (catch Exception _ [catalog 0])))

;; ----------------------------------------------------- copy / paste settings

(def setting-groups
  "Settings by panel, for copying them between frames."
  {:tone    [:exposure :contrast :highlights :shadows :whites :blacks :temp :tint]
   :color   [:vibrance :saturation :hsl :split-sh-hue :split-sh-sat :split-hl-hue :split-hl-sat :split-balance]
   :detail  [:texture :clarity :dehaze :sharpen :sharpen-radius :sharpen-masking :denoise :denoise-color]
   :curve   [:curve :curve-r :curve-g :curve-b]
   :effects [:fade :bw :grain :vignette]
   :geometry (vec geometry/geometry-keys)
   :retouch [:spots :local]})

(def default-copy-groups [:tone :color :detail :curve :effects])

(defn copy-settings
  "The frame's settings in the chosen groups (default: everything that is not
  the picture's geometry or retouching), as a map to paste elsewhere."
  ([catalog path] (copy-settings catalog path default-copy-groups))
  ([catalog path groups]
   (select-keys (adj catalog path) (mapcat setting-groups groups))))

(defn paste-settings
  "Applies copied settings to the frame, as one history step."
  [catalog path settings]
  (-> catalog
      (update-frame path #(update % :adj (fn [a] (merge pipeline/default-settings a settings))))
      (commit path "PASTE SETTINGS")))

;; ---------------------------------------------- virtual copies and removal

(defn add-virtual-copy
  "Adds a virtual copy of `path` to shoot `shoot-id`: the same file with its own
  edits, starting from the original's. Returns [catalog new-id]."
  [catalog shoot-id path]
  (let [base (paths/source-file path)
        taken (into #{} (keep paths/copy-number) (mapcat :paths (:shoots catalog)))
        n (inc (reduce max 0 taken))
        id (paths/copy-id base n)
        f (frame catalog path)
        cur (merge pipeline/default-settings (:adj f))
        catalog (assoc-in catalog [:frames id]
                          (assoc (select-keys f [:rating :colour :keywords :meta])
                                 :adj cur :history [{:label "VIRTUAL COPY" :adj cur}] :hpos 0))]
    [(update catalog :shoots
             (fn [shoots]
               (mapv (fn [sh]
                       (if (= shoot-id (:id sh))
                         (update sh :paths
                                 (fn [ps]
                                   (let [last-of-base (last (keep-indexed #(when (= base (paths/source-file %2)) %1) ps))
                                         at (if last-of-base (inc last-of-base) (count ps))]
                                     (vec (concat (take at ps) [id] (drop at ps))))))
                         sh))
                     shoots)))
     id]))

(defn remove-frame
  "Takes `path` out of a shoot (the file is never touched). Its edits are
  forgotten when no shoot lists it any more."
  [catalog shoot-id path]
  (let [catalog (update catalog :shoots
                        (fn [shoots] (mapv #(if (= shoot-id (:id %)) (update % :paths (fn [ps] (vec (remove #{path} ps)))) %) shoots)))]
    (if (some #{path} (mapcat :paths (:shoots catalog)))
      catalog
      (update catalog :frames dissoc path))))

;; ---------------------------------------------------------- search and sort

(defn- text-of
  "Lower-case text a search matches: file name, title, caption, creator,
  keywords and camera details."
  [catalog path meta]
  (let [f (frame catalog path)]
    (str/lower-case
      (str/join " " (concat [(.getName (File. (paths/source-file path)))]
                            (vals (select-keys (:meta f) meta-fields))
                            (:keywords f)
                            (keep #(some-> (meta %) str) [:make :model :lens-model :datetime-original]))))))

(defn- natural-key
  "Splits a name into text and number chunks so IMG_2 sorts before IMG_10."
  [name]
  (mapv #(if (Character/isDigit (.charAt ^String % 0)) [0 (Long/parseLong %)] [1 %])
        (re-seq #"\d{1,18}|\D+" (str/lower-case name))))

(defn query
  "The frames of `paths` matching `opts`, in the requested order.
    :text        every word must appear in the frame's searchable text
    :min-rating  at least this many stars
    :colour      only this colour label
    :keyword     only frames with this keyword
    :rejected    :hide (default), :show or :only
    :edited      true = only edited frames
    :sort        :name (default), :rating, :edited, :capture, :import (shoot order)
    :dir         :asc (default) or :desc
  `meta-fn` maps a path to its camera metadata (see exif/read-tags); it is only
  consulted for text search and capture-time sorting."
  ([catalog paths opts] (query catalog paths opts (constantly nil)))
  ([catalog paths {:keys [text min-rating colour keyword rejected edited sort dir]
                   :or {min-rating 0 rejected :hide sort :import dir :asc}}
    meta-fn]
   (let [words (remove str/blank? (str/split (str/lower-case (str text)) #"\s+"))
         kw (some-> keyword str str/trim str/lower-case not-empty)
         keep? (fn [p]
                 (and (>= (rating catalog p) min-rating)
                      (case rejected :hide (not (rejected? catalog p)) :only (rejected? catalog p) true)
                      (or (nil? colour) (= colour (:colour (frame catalog p))))
                      (or (nil? kw) (some #{kw} (keywords catalog p)))
                      (or (not edited) (edited? catalog p))
                      (or (empty? words)
                          (let [t (text-of catalog p (or (meta-fn p) {}))] (every? #(str/includes? t %) words)))))
         kept (filterv keep? paths)
         keyfn (case sort
                 :name (fn [p] (natural-key (.getName (File. (paths/source-file p)))))
                 :rating (fn [p] (rating catalog p))
                 :edited (fn [p] (if (edited? catalog p) 1 0))
                 :capture (fn [p] (str (:datetime-original (or (meta-fn p) {}))))
                 nil)
         cmp (if (= dir :desc) (fn [a b] (compare b a)) compare)]
     (if keyfn (vec (sort-by keyfn cmp kept)) (if (= dir :desc) (vec (reverse kept)) kept)))))

;; ----------------------------------------------------------------- XMP import

(defn apply-xmp-data
  "Applies what a sidecar carried (see darkroom.imaging.xmp/parse-xmp) to a frame:
  rating, colour label, keywords, notes and, when the sidecar holds this app's
  settings, the edits (as the frame's starting history entry)."
  [catalog path {:keys [rating colour keywords meta adj]}]
  (update-frame catalog path
                (fn [f]
                  (cond-> f
                    rating   (assoc :rating (max 0 (min 5 (long rating))))
                    colour   (assoc :colour colour)
                    (seq keywords) (assoc :keywords (vec (sort (distinct (concat (:keywords f) keywords)))))
                    (seq meta) (update :meta merge (select-keys meta meta-fields))
                    adj      (as-> f f
                               (let [a (merge pipeline/default-settings adj)]
                                 (assoc f :adj a :history [{:label "IMPORTED XMP" :adj a}] :hpos 0)))))))

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

(defn default-file
  "The catalog file: `-Dgroovehaus.catalog=<file>` or the GROOVEHAUS_CATALOG
  environment variable when set (point it into a Dropbox / iCloud / Syncthing
  folder to share one catalog between computers), else catalog.edn in the data folder."
  ^File []
  (if-let [p (not-empty (or (System/getProperty "groovehaus.catalog") (System/getenv "GROOVEHAUS_CATALOG")))]
    (File. ^String p)
    (File. (app-dir) "catalog.edn")))

(defn changed-on-disk?
  "True when `file` exists and was modified after `seen-mtime` (a long, 0 when
  the file had not been seen), i.e. something else wrote it."
  [^File file seen-mtime]
  (and (.exists file) (not= (.lastModified file) (long seen-mtime))))

(defn set-aside!
  "Copies `file` to `file.conflict-<millis>` (kept next to it) and returns that File."
  ^File [^File file]
  (let [aside (File. (str file ".conflict-" (System/currentTimeMillis)))]
    (Files/copy (.toPath file) (.toPath aside) (into-array java.nio.file.CopyOption [StandardCopyOption/REPLACE_EXISTING]))
    aside))

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
