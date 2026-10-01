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
    :hist-rgb  histogram shows R, G, B instead of luma
    :clip-view paint clipped pixels on the canvas (red highlights, blue shadows)
    :pick      nil or :wb while the white-balance picker waits for a click
    :local-sel id of the selected local-adjustment layer (LOCAL tab); :local-mask shows its mask
    :tool      brush / spot settings {:brush-size :brush-feather :brush-flow :erase :spot-size :spot-mode}
    :exporting export overlay open?
    :fmt :size :q :cspace :export-dir   export options (fmt :jpeg :png :tiff :webp; cspace: :srgb :display-p3 :adobe-rgb)
    :export-opts  {:template :sharpen :sharpen-amount :metadata :wm-text :wm-pos :scope} (see ui/export-overlay)
    :adding / :toast            transient UI
    :query     library search/sort/filter {:text :min-rating :colour :keyword :sort :dir :rejected}
    :sel       set of multi-selected frames (library); actions use `selection`
    :clipboard settings copied with copy-settings!
    :watch     folder being watched for new frames (hot folder), or nil
    :meta-rev  bumped when camera metadata finishes loading (re-sorts/re-searches)"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [darkroom.catalog :as cat]
            [darkroom.imaging.browser :as browser]
            [darkroom.imaging.camera :as camera]
            [darkroom.imaging.exif :as exif]
            [darkroom.imaging.lens :as lens]
            [darkroom.imaging.local :as local]
            [darkroom.imaging.watch :as watch]
            [darkroom.imaging.xmp :as xmp])
  (:import (java.io File)
           (javafx.application Platform)
           (java.util.concurrent Executors ScheduledExecutorService ScheduledFuture ThreadFactory TimeUnit)))

(defonce state
  (atom {:catalog cat/empty-catalog :view :library :shoot nil :cur nil :tab :basic
         :lf :all :tsz 220 :before false :hist-rgb false :clip-view false :pick nil :local-sel nil :local-mask false
         :tool {:brush-size 0.04 :brush-feather 0.5 :brush-flow 1.0 :erase false :spot-size 0.02 :spot-mode :heal} :exporting false :fmt :jpeg :size 2048 :q 90 :cspace :srgb
         :export-dir nil :adding false :toast nil
         :export-opts {:template "groovehaus_{name}" :sharpen nil :sharpen-amount :standard :metadata :all
                       :wm-text "" :wm-pos :bottom-right :scope :frame}
         :query {} :sel #{} :survey false :clipboard nil :meta-rev 0}))

;; ------------------------------------------------------------- derivations

(defn frames
  "Paths in the current shoot."
  ([] (frames @state))
  ([st] (vec (:paths (cat/shoot (:catalog st) (:shoot st))))))

(defn cur-path [st] (:cur st))

(defn cur-adj [st] (when-let [p (:cur st)] (cat/adj (:catalog st) p)))

(defonce exif-cache (atom {}))

(defn meta-of
  "Cached camera metadata (see exif/read-tags) of a frame, or nil until loaded."
  [path] (get @exif-cache path))

(defn query-of
  "The cat/query options for state `st`: the search/sort fields plus the legacy
  ALL / PICKS / EDITED / REJECTED filter."
  [st]
  (let [q (merge {:rejected :hide} (:query st))]
    (case (:lf st)
      :picks    (assoc q :min-rating 5)
      :edited   (assoc q :edited true)
      :rejected (assoc q :rejected :only)
      q)))

(defn visible-frames
  "Frames shown in the library after the filters, search and sort."
  [st]
  (cat/query (:catalog st) (frames st) (query-of st) meta-of))

(defn survey?
  "True when the library shows only the selected frames, large (needs at least two)."
  [st]
  (and (:survey st) (> (count (:sel st)) 1)))

(defn grid-frames
  "The frames the library grid shows: the filtered frames, or in survey just the selected ones."
  [st]
  (let [vis (visible-frames st)]
    (if (survey? st) (filterv (:sel st) vis) vis)))

(defn selection
  "The frames actions apply to: the multi-selection in shoot order, else the
  current frame, else none."
  ([] (selection @state))
  ([st] (let [sel (:sel st)]
          (cond (seq sel) (filterv sel (frames st))
                (:cur st) [(:cur st)]
                :else []))))

;; ----------------------------------------------------------------- persist

(defonce ^:private ^ScheduledExecutorService saver
  (Executors/newSingleThreadScheduledExecutor
    (reify ThreadFactory (newThread [_ r] (doto (Thread. ^Runnable r "darkroom-save") (.setDaemon true))))))

(defonce ^:private pending-save (atom nil))
(defonce catalog-file (atom (cat/default-file)))
(defonce ^:private seen-mtime (atom 0))

(declare toast!)

(defn- run-ui!
  "Runs `f` on the FX thread (directly when the toolkit is not running, e.g. in tests)."
  [f]
  (try (Platform/runLater f) (catch IllegalStateException _ (f))))

(defn- write-catalog!
  "Saves `c`. If another program or computer changed the file since this one
  last read or wrote it (a shared sync folder), the other version is first set
  aside as catalog.edn.conflict-<time> so nothing is lost silently."
  [c]
  (let [^File f @catalog-file]
    (when (cat/changed-on-disk? f @seen-mtime)
      (let [aside (cat/set-aside! f)]
        (run-ui! #(toast! (str "CATALOG CHANGED ELSEWHERE — OLD COPY KEPT AS " (.getName aside))))))
    (cat/save! c f)
    (reset! seen-mtime (.lastModified f))))

(defn save-soon!
  "Writes the catalog about 600 ms after the last change (debounced)."
  []
  (when-let [^ScheduledFuture f @pending-save] (.cancel f false))
  (let [c (:catalog @state)]
    (reset! pending-save
            (.schedule saver
                       ^Runnable (fn []
                                   (try (write-catalog! c)
                                        (catch Throwable t
                                          (binding [*out* *err*] (println "catalog save failed:" (.getMessage t))))))
                       600 TimeUnit/MILLISECONDS))))

(defn save-now!
  "Flushes the catalog synchronously (use on exit)."
  []
  (when-let [^ScheduledFuture f @pending-save] (.cancel f false))
  (try (write-catalog! (:catalog @state)) (catch Throwable _ nil)))

(defn update-catalog! [f & args]
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

(defn select!
  "Makes `path` the current frame and the only selected one."
  [path]
  (swap! state assoc :cur path :sel #{}))

(defn toggle-select!
  "Ctrl/Cmd-click: adds or removes a frame from the selection (the current
  frame counts as selected)."
  [path]
  (swap! state
         (fn [st]
           (let [sel (if (seq (:sel st)) (:sel st) (if (:cur st) #{(:cur st)} #{}))
                 sel (if (sel path) (disj sel path) (conj sel path))]
             (assoc st :sel sel :cur (if (sel path) path (or (first (filter sel (frames st))) (:cur st))))))))

(defn range-select!
  "Shift-click: selects the visible frames between the current frame and `path`."
  [path]
  (swap! state
         (fn [st]
           (let [vis (visible-frames st)
                 a (.indexOf ^java.util.List vis (:cur st)) b (.indexOf ^java.util.List vis path)]
             (if (or (neg? a) (neg? b))
               (assoc st :cur path :sel #{})
               (assoc st :sel (set (subvec vis (min a b) (inc (max a b))))))))))

(defn clear-selection! [] (swap! state assoc :sel #{}))

(defn select-all!
  "Selects every visible frame."
  []
  (swap! state (fn [st] (assoc st :sel (set (visible-frames st))))))

(defn set-query!
  "Sets one library search field (:text :sort :dir :colour); nil clears it."
  [k v]
  (swap! state update :query (fn [q] (if (nil? v) (dissoc q k) (assoc q k v)))))

(defn nav!
  "Previous/next frame within the shoot, wrapping."
  [d]
  (swap! state
          (fn [st]
            (let [fs (frames st)]
              (if (empty? fs)
                st
                (let [k (max 0 (.indexOf ^java.util.List fs (:cur st)))]
                  (assoc st :cur (fs (mod (+ k d) (count fs))) :sel #{})))))))

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
                 (assoc st :cur (vis k) :sel #{})))))))

(defn select-shoot! [id]
  (swap! state
         (fn [st]
           (let [fs (vec (:paths (cat/shoot (:catalog st) id)))]
             (assoc st :shoot id :lf :all :query {} :sel #{} :cur (or (first fs) (:cur st)))))))

(defn create-shoot!
  "Creates a (possibly empty) shoot and makes it current. Returns its id."
  [name paths]
  (let [[c id] (cat/add-shoot (:catalog @state) name paths)]
    (when id
      (swap! state assoc :catalog c :shoot id :adding false :lf :all :query {} :sel #{}
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
  Selects the first newly added frame and shows the Library, unless
  `:keep-view? true` (hot-folder imports: select the newest, stay where you are).
  Returns the number of new frames."
  [paths & [{:keys [keep-view?]}]]
  (let [paths (vec (distinct paths))]
    (when (seq paths)
      (when-not (:shoot @state) (create-shoot! "FIRST SHOOT" []))
      (let [id     (:shoot @state)
            before (set (frames))]
        (update-catalog! cat/add-paths id paths)
        (let [new (vec (remove before paths))]
          ;; a sidecar next to a new file brings its rating, label, keywords and edits
          (when (seq new)
            (update-catalog! (fn [c] (reduce (fn [c p] (if-let [x (xmp/read-sidecar p)] (cat/apply-xmp-data c p x) c)) c new))))
          (when (seq new)
            (swap! state (fn [st] (cond-> (assoc st :cur (if keep-view? (last new) (first new)))
                                    (not keep-view?) (assoc :view :library)))))
          (count new))))))

(defn rate!
  "Rates the selected frames (or the current one). Choosing the rating they all
  already have clears it."
  [n]
  (let [ps (selection)]
    (when (seq ps)
      (let [all? (every? #(= n (cat/rating (:catalog @state) %)) ps)
            target (if all? 0 n)]
        (update-catalog! (fn [c] (reduce #(cat/assign-rating %1 %2 target) c ps)))))))

(defn reject!
  "Toggles the reject flag of the selected frames (all rejected if any is not)."
  []
  (let [ps (selection)]
    (when (seq ps)
      (let [all? (every? #(cat/rejected? (:catalog @state) %) ps)]
        (update-catalog! (fn [c] (reduce #(if (= all? (cat/rejected? %1 %2)) (cat/toggle-reject %1 %2) %1) c ps)))))))

(defn label!
  "Sets (or, when they all have it already, clears) a colour label on the selection."
  [colour]
  (let [ps (selection)]
    (when (seq ps)
      (let [all? (every? #(= colour (cat/colour (:catalog @state) %)) ps)]
        (update-catalog! (fn [c] (reduce #(let [has? (= colour (cat/colour %1 %2))]
                                            (if all? (if has? (cat/set-colour %1 %2 colour) %1) (if has? %1 (cat/set-colour %1 %2 colour))))
                                         c ps)))))))

(defn add-keywords! [text]
  (let [ps (selection)] (when (seq ps) (update-catalog! (fn [c] (reduce #(cat/add-keywords %1 %2 text) c ps))))))

(defn remove-keyword! [kw]
  (let [ps (selection)] (when (seq ps) (update-catalog! (fn [c] (reduce #(cat/remove-keyword %1 %2 kw) c ps))))))

(defn set-meta! [k v]
  (let [ps (selection)] (when (seq ps) (update-catalog! (fn [c] (reduce #(cat/set-meta %1 %2 k v) c ps))))))

(defn set-meta-if-changed!
  "set-meta! for the selection, skipped when the current frame already has `v`
  (focus-lost commits fire even when nothing was typed)."
  [k v]
  (when-let [p (:cur @state)]
    (when (not= (clojure.string/trim (str v)) (str (get (cat/frame-meta (:catalog @state) p) k)))
      (set-meta! k v))))

(defn undo! [] (when-let [p (:cur @state)] (update-catalog! cat/undo p)))
(defn redo! [] (when-let [p (:cur @state)] (update-catalog! cat/redo p)))

(defn toggle-pick! [path] (update-catalog! cat/toggle-pick path))

(defn set-adj!
  "Live adjustment of the current frame (no history entry yet)."
  [k v]
  (when-let [p (:cur @state)] (swap! state update :catalog cat/set-adj p k v)))

(defn set-adjs!
  "Live adjustment of several settings of the current frame at once (one state
  change, so one render); `m` is {setting value}."
  [m]
  (when-let [p (:cur @state)]
    (swap! state update :catalog (fn [c] (reduce-kv (fn [c k v] (cat/set-adj c p k v)) c m)))))

(declare commit!)

(defn layers
  "The current frame's local-adjustment layers."
  ([] (layers @state))
  ([st] (vec (:local (cur-adj st)))))

(defn selected-layer
  "The selected local-adjustment layer of the current frame, or nil."
  ([] (selected-layer @state))
  ([st] (let [id (:local-sel st)] (first (filter #(= id (:id %)) (layers st))))))

(defn select-layer! [id] (swap! state assoc :local-sel id))

(defn add-layer!
  "Adds a new local-adjustment layer of `type` and selects it."
  [type]
  (let [ls (layers) id (local/next-id ls)]
    (set-adj! :local (conj ls (local/new-layer type id)))
    (select-layer! id)
    (commit! (str "ADD " (local/type-labels type)))))

(defn update-layer!
  "Live change of layer `id` (no history entry until `commit!`): f maps the
  layer to its new value."
  [id f]
  (set-adj! :local (mapv #(if (= id (:id %)) (f %) %) (layers))))

(defn delete-layer! [id]
  (set-adj! :local (vec (remove #(= id (:id %)) (layers))))
  (when (= id (:local-sel @state)) (select-layer! nil))
  (commit! "DELETE LAYER"))

(defn spots [] (vec (:spots (cur-adj @state))))

(defn add-spot!
  "Adds a spot {:x :y :r :mode ...} (fractions of the cropped picture)."
  [spot]
  (set-adj! :spots (conj (spots) spot))
  (commit! (if (= :clone (:mode spot)) "CLONE SPOT" "HEAL SPOT")))

(defn delete-spot! [i]
  (set-adj! :spots (vec (concat (take i (spots)) (drop (inc i) (spots)))))
  (commit! "DELETE SPOT"))

(defn clear-spots! []
  (set-adj! :spots [])
  (commit! "CLEAR SPOTS"))

(defn set-tool!
  "Sets one brush / spot tool setting, e.g. (set-tool! :brush-size 0.05)."
  [k v]
  (swap! state assoc-in [:tool k] v))

(defn commit! [label]
  (when-let [p (:cur @state)] (update-catalog! cat/commit p label)))

(defn reset-adj! [k label default]
  (set-adj! k default)
  (commit! (str label " RESET")))

(defn apply-preset!
  "Applies a built-in preset to the selected frames."
  [preset-name]
  (let [ps (selection)] (when (seq ps) (update-catalog! (fn [c] (reduce #(cat/apply-preset %1 %2 preset-name) c ps))))))

(defn apply-user-preset! [preset-name]
  (let [ps (selection)] (when (seq ps) (update-catalog! (fn [c] (reduce #(cat/apply-user-preset %1 %2 preset-name) c ps))))))

(defn save-preset!
  "Saves the current frame's settings as a user preset."
  [name]
  (when-let [p (:cur @state)]
    (when (cat/shoot-name name)
      (update-catalog! cat/save-preset p name)
      true)))

(defn delete-preset! [name] (update-catalog! cat/delete-preset name))

(defn export-presets!
  "Writes the user's presets to `file`. Returns the number written."
  [^File file]
  (spit file (cat/presets->edn (:catalog @state)))
  (count (cat/user-presets (:catalog @state))))

(defn import-presets!
  "Reads presets from `file`. Returns how many were added."
  [^File file]
  (let [[c n] (cat/import-presets (:catalog @state) (slurp file))]
    (when (pos? n) (update-catalog! (constantly c)))
    n))

(defn copy-settings!
  "Copies the current frame's settings (not its geometry or retouching)."
  []
  (when-let [p (:cur @state)]
    (swap! state assoc :clipboard (cat/copy-settings (:catalog @state) p))
    true))

(defn paste-settings!
  "Pastes the copied settings onto the selected frames. Returns how many."
  []
  (when-let [clip (:clipboard @state)]
    (let [ps (selection)]
      (update-catalog! (fn [c] (reduce #(cat/paste-settings %1 %2 clip) c ps)))
      (count ps))))

(defn add-snapshot! [name]
  (when-let [p (:cur @state)] (when-not (clojure.string/blank? name) (update-catalog! cat/add-snapshot p name) true)))
(defn apply-snapshot! [i] (when-let [p (:cur @state)] (update-catalog! cat/apply-snapshot p i)))
(defn delete-snapshot! [i] (when-let [p (:cur @state)] (update-catalog! cat/delete-snapshot p i)))

(defn virtual-copy!
  "Makes a virtual copy of each selected frame; the last one becomes current."
  []
  (let [ps (selection) shoot (:shoot @state)]
    (when (and shoot (seq ps))
      (let [[c ids] (reduce (fn [[c ids] p] (let [[c' id] (cat/add-virtual-copy c shoot p)] [c' (conj ids id)])) [(:catalog @state) []] ps)]
        (swap! state assoc :catalog c :cur (last ids) :sel #{})
        (save-soon!)
        (count ids)))))

(defn remove-frames!
  "Takes the selected frames out of the shoot (files are never touched)."
  []
  (let [ps (selection) shoot (:shoot @state)]
    (when (and shoot (seq ps))
      (let [fs (frames) next-cur (first (remove (set ps) (concat (drop-while #(not= % (first ps)) fs) fs)))]
        (swap! state (fn [st] (assoc st :catalog (reduce #(cat/remove-frame %1 shoot %2) (:catalog st) ps) :sel #{} :cur next-cur)))
        (save-soon!)
        (count ps)))))

;; --------------------------------------------------------- camera profiles

(defonce profile-info (atom {})) ; .dcp path -> {:name :camera} (or {:error msg})

(defn- read-profile-info!
  "Reads the .dcp at `path`, remembers its name and camera, and returns that info
  ({:error message} when it is not a usable profile)."
  [path]
  (let [info (try (let [p (camera/load-profile path)] {:name (or (:name p) (.getName (File. ^String path))) :camera (:camera p)})
                  (catch Throwable t {:error (or (.getMessage t) "unreadable")}))]
    (swap! profile-info assoc path info)
    info))

(defn camera-profiles
  "The camera profiles the catalog knows: [{:path :name :camera}] (unreadable ones are left out)."
  ([] (camera-profiles @state))
  ([st]
   (vec (for [p (:camera-profiles (:catalog st))
              :let [i (get @profile-info p)]
              :when (and i (not (:error i)))]
          (assoc i :path p)))))

(defn add-camera-profiles!
  "Adds the .dcp files to the catalog's profile list. Returns {:added n :failed [file names]}."
  [files]
  (let [paths (mapv #(.getPath ^File %) files)
        results (mapv (fn [p] [p (read-profile-info! p)]) paths)
        ok (mapv first (remove (comp :error second) results))]
    (when (seq ok)
      (update-catalog! (fn [c] (assoc c :camera-profiles (vec (distinct (concat (:camera-profiles c) ok)))))))
    {:added (count ok) :failed (mapv #(.getName (File. ^String (first %))) (filter (comp :error second) results))}))

(defn remove-camera-profile!
  "Forgets a profile (frames that use it fall back to the default decode)."
  [path]
  (update-catalog! (fn [c]
                     (let [c (assoc c :camera-profiles (vec (remove #{path} (:camera-profiles c))))]
                       (reduce (fn [c p] (if (= path (:camera-profile (cat/adj c p)))
                                           (-> c (cat/set-adj p :camera-profile nil) (cat/commit p "CAMERA PROFILE OFF"))
                                           c))
                               c (keys (:frames c)))))))

(defn set-camera-profile!
  "Gives the selected frames the camera profile at `path` (nil = the default decode); one history step each."
  [path]
  (let [ps (selection)]
    (when (seq ps)
      (update-catalog! (fn [c] (reduce (fn [c p] (-> c (cat/set-adj p :camera-profile path)
                                                     (cat/commit p (if path "CAMERA PROFILE" "CAMERA PROFILE OFF"))))
                                       c ps))))))

(defn toggle-profile-curve!
  "Switches the profile's own tone curve on or off for the selected frames."
  []
  (let [ps (selection)]
    (when (seq ps)
      (let [on? (not (:camera-profile-curve (cat/adj (:catalog @state) (first ps))))]
        (update-catalog! (fn [c] (reduce (fn [c p] (-> c (cat/set-adj p :camera-profile-curve on?)
                                                       (cat/commit p (if on? "PROFILE CURVE ON" "PROFILE CURVE OFF"))))
                                         c ps)))))))

(defn auto-camera-profile!
  "Picks the profile made for the current frame's camera (EXIF make + model against the
  profile's unique camera model) and applies it. Returns the profile entry or nil, saying why."
  []
  (when-let [p (:cur @state)]
    (let [tags (or (meta-of p) (exif/read-tags p))
          want (set (map #(str/lower-case (str/trim %))
                         (remove str/blank? [(:model tags) (str (:make tags) " " (:model tags))])))
          hit (first (filter #(some-> (:camera %) str/lower-case str/trim want) (camera-profiles)))]
      (cond (empty? (camera-profiles)) (do (toast! "ADD A CAMERA PROFILE (.DCP) FIRST") nil)
            (nil? hit) (do (toast! (str "NO PROFILE FOR " (str/upper-case (str (:model tags))))) nil)
            :else (do (set-camera-profile! (:path hit)) (toast! (str "APPLIED " (str/upper-case (:name hit)))) hit)))))

;; ------------------------------------------------------------- lens profiles

(defonce lens-db (atom []))

(defn load-lens-db!
  "Reads the lensfun XML files in `dir` on a background thread, remembers the
  folder in the catalog, and toasts the number of lenses found."
  [^File dir]
  (update-catalog! assoc :lens-db-dir (.getPath dir))
  (doto (Thread. ^Runnable
                 (fn []
                   (let [db (lens/load-database dir)]
                     (reset! lens-db db)
                     (run-ui! #(do (swap! state update :meta-rev inc)
                                   (toast! (if (seq db) (str (count db) " LENSES LOADED") "NO LENS PROFILES FOUND IN THAT FOLDER"))))))
                 "darkroom-lens-db")
    (.setDaemon true) (.start)))

(defn load-saved-lens-db!
  "At startup: reloads the lens database from the folder the catalog remembers."
  []
  (when-let [d (:lens-db-dir (:catalog @state))]
    (let [f (File. ^String d)]
      (when (.isDirectory f)
        (doto (Thread. ^Runnable (fn [] (reset! lens-db (lens/load-database f))) "darkroom-lens-db")
          (.setDaemon true) (.start))))))

(defn apply-lens-profile!
  "Looks the current frame's lens up in the lens database and applies its
  distortion / CA profile (one history step). Returns the profile, or nil after
  saying why there is none."
  []
  (when-let [p (:cur @state)]
    (let [tags (or (meta-of p) (exif/read-tags p))
          prof (lens/profile-for @lens-db tags)
          fail! (fn [msg] (toast! msg) nil)]
      (cond
        (empty? @lens-db) (fail! "LOAD A LENS DATABASE FIRST")
        (str/blank? (:lens-model tags)) (fail! "THIS FILE DOES NOT NAME ITS LENS")
        (nil? (:focal-length tags)) (fail! "THIS FILE HAS NO FOCAL LENGTH")
        (nil? prof) (fail! (str "NO PROFILE FOR " (str/upper-case (:lens-model tags))))
        :else (do (set-adj! :lens-profile prof)
                  (commit! "LENS PROFILE")
                  (toast! (str "APPLIED " (str/upper-case (:name prof))))
                  prof)))))

(defn clear-lens-profile! []
  (when (:lens-profile (cur-adj @state))
    (set-adj! :lens-profile nil)
    (commit! "LENS PROFILE OFF")))

;; ---------------------------------------------------------------- hot folder

(defonce ^:private stop-watch (atom nil))

(defn stop-watching!
  "Stops the hot folder, if one is watched."
  []
  (when-let [stop @stop-watch] (reset! stop-watch nil) (stop))
  (swap! state assoc :watch nil))

(defn watch-folder!
  "Imports every image that appears in `dir` into the current shoot as it
  finishes being written (tethered-style capture through a camera's save-to-folder
  setting). The newest frame becomes current; the view does not change. Replaces
  any earlier watch. Returns the folder."
  [^File dir]
  (stop-watching!)
  (when (.isDirectory dir)
    (reset! stop-watch
            (watch/start! dir (fn [paths]
                                (run-ui!
                                  (fn []
                                    (let [n (import-paths! paths {:keep-view? true})]
                                      (when (pos? (or n 0))
                                        (toast! (str n " NEW FRAME" (when (not= n 1) "S") " FROM FOLDER"))))))))) 
    (swap! state assoc :watch (.getPath dir))
    dir))

;; ------------------------------------------------------------ metadata / XMP

(defonce ^:private meta-reader (atom nil)) ; :starting while a reader thread runs

(defn load-meta!
  "Reads camera metadata of the current shoot's frames on a background thread
  into `exif-cache`, bumping :meta-rev as it goes so search and sort update. One
  reader runs at a time; it keeps going until nothing is left to read."
  []
  (when (and (seq (remove #(contains? @exif-cache %) (frames)))
             (compare-and-set! meta-reader nil :starting))
    (doto (Thread. ^Runnable
                   (fn []
                     (try
                       (loop []
                         (let [todo (remove #(contains? @exif-cache %) (frames))]
                           (when (seq todo)
                             (doseq [[i p] (map-indexed vector todo)]
                               (swap! exif-cache assoc p (exif/read-tags p))
                               (when (zero? (mod (inc i) 25)) (swap! state update :meta-rev inc)))
                             (swap! state update :meta-rev inc)
                             (recur))))
                       (finally (reset! meta-reader nil))))
                   "darkroom-exif")
      (.setDaemon true) (.start))))

(defn- xmp-data [catalog path]
  (let [f (cat/frame catalog path)]
    {:rating (:rating f) :colour (:colour f) :keywords (:keywords f) :meta (:meta f) :adj (cat/adj catalog path)}))

(defn write-xmp!
  "Writes an XMP sidecar for each frame of the current shoot (virtual copies
  share their file's sidecar, so only original files are written). Returns the
  number written; failures are skipped."
  []
  (let [c (:catalog @state)]
    (count (keep (fn [p] (try (xmp/write-sidecar! p (xmp-data c p)) (catch Exception _ nil)))
                 (remove darkroom.imaging.paths/virtual-copy? (frames))))))



(defn revert! [i]
  (when-let [p (:cur @state)] (update-catalog! cat/revert-to p i)))

;; ----------------------------------------------------------------- startup

(defn load-catalog!
  "Loads the saved catalog and picks a starting shoot/frame."
  []
  (let [c (cat/load! @catalog-file)
        s (first (:shoots c))]
    (reset! seen-mtime (.lastModified ^File @catalog-file))
    (swap! state assoc :catalog c :shoot (:id s) :cur (first (:paths s)))
    (doseq [pp (:camera-profiles c)] (read-profile-info! pp))
    (load-saved-lens-db!)))

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
