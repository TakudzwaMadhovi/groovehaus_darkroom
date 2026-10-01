(ns darkroom.ui.thumbs
  "Lazy, cached thumbnails for library tiles, shoot covers and the filmstrip.
  Thumbnails are generated on a small background pool and kept in a bounded
  in-memory LRU cache keyed by path + modification time."
  (:require [darkroom.imaging.browser :as browser]
            [darkroom.imaging.paths :as paths]
            [darkroom.ui.fx :as fx])
  (:import (java.io File)
           (java.util LinkedHashMap Map Map$Entry)
           (java.util.concurrent ExecutorService)
           (javafx.application Platform)))

(def side
  "Longest side of a generated thumbnail (design: 480 px)."
  480)

(defonce ^:private ^ExecutorService pool
  (fx/daemon-executor "darkroom-thumbs" (max 1 (min 2 (quot (.availableProcessors (Runtime/getRuntime)) 2)))))

(defn- lru [limit]
  (java.util.Collections/synchronizedMap
    (proxy [LinkedHashMap] [64 0.75 true]
      (removeEldestEntry [^Map$Entry _] (> (.size ^LinkedHashMap this) limit)))))

(defonce ^:private ^Map cache (lru 200))

(defn- key-of [path]
  (let [^File f (File. (paths/source-file path))]
    [(.getPath f) (.lastModified f) side]))

(defn cached
  "The cached JavaFX Image for `path`, or nil."
  [path]
  (.get cache (key-of path)))

(defn request!
  "Delivers the thumbnail for `path` to `(deliver! image)` on the FX thread:
  immediately if cached, else after generating it on the pool. `wanted?` is
  checked just before work starts so recycled tiles skip it. Failures are
  logged and nothing is delivered."
  [path wanted? deliver!]
  (if-let [img (cached path)]
    (deliver! img)
    (.execute pool
              (fn []
                (when (wanted?)
                  (try
                    (let [img (or (cached path) (fx/->fx-image (browser/thumbnail path side)))]
                      (.put cache (key-of path) img)
                      (Platform/runLater #(deliver! img)))
                    (catch Throwable t
                      (binding [*out* *err*]
                        (println "thumbnail failed:" (str path) (.getMessage t))))))))))
