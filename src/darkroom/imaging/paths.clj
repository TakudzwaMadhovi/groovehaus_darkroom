(ns darkroom.imaging.paths
  "Frames are identified in the catalog by a path string. A virtual copy of a
  frame (same file, its own edits) adds a '#vcN' suffix to that string; this
  namespace maps an identifier back to the file it reads from. Pure logic.")

(def ^:private suffix #"#vc(\d+)$")

(defn source-file
  "The file path behind a frame identifier (the identifier itself for an
  ordinary frame)."
  ^String [id]
  (clojure.string/replace (str id) suffix ""))

(defn copy-number
  "N of a virtual copy identifier 'file#vcN', or nil for an ordinary frame."
  [id]
  (some-> (re-find suffix (str id)) second Long/parseLong))

(defn virtual-copy? [id] (boolean (copy-number id)))

(defn copy-id
  "The identifier of virtual copy number `n` of `id`'s file."
  [id n]
  (str (source-file id) "#vc" n))
