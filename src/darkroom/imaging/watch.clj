(ns darkroom.imaging.watch
  "Hot folder: watches a directory and reports new image files once they have
  finished being written. This is how tethered shooting works with camera
  software that saves each frame into a folder (the camera itself is not
  controlled). Pure logic, no UI dependency."
  (:require [darkroom.imaging.browser :as browser])
  (:import (java.io File)
           (java.nio.file FileSystems Path StandardWatchEventKinds WatchKey WatchService)
           (java.util.concurrent TimeUnit)))

(def ^:private settle-ms 400)

(defn- settled?
  "True when `file` has a non-zero size that did not change over `settle-ms`
  (a camera or copy is still writing otherwise)."
  [^File f]
  (let [a (.length f)]
    (Thread/sleep settle-ms)
    (and (pos? a) (.exists f) (= a (.length f)))))

(defn start!
  "Watches `dir` (not its sub-folders). `on-files` is called on a background
  thread with a vector of new supported image paths each time some finish
  arriving. Returns a no-arg fn that stops watching."
  [^File dir on-files]
  (let [^WatchService ws (.newWatchService (FileSystems/getDefault))
        path (.toPath dir)
        running (atom true)
        _ (.register path ws (into-array java.nio.file.WatchEvent$Kind [StandardWatchEventKinds/ENTRY_CREATE
                                                                          StandardWatchEventKinds/ENTRY_MODIFY]))
        seen (atom #{})
        t (doto (Thread.
                  ^Runnable
                  (fn []
                    (try
                      (while @running
                        (when-let [^WatchKey k (.poll ws 500 TimeUnit/MILLISECONDS)]
                          (let [fs (->> (.pollEvents k)
                                        (keep (fn [e] (when-let [^Path p (.context e)] (File. dir (str p)))))
                                        (filter #(and (.isFile ^File %) (browser/supported-image? %)))
                                        (map #(.getPath ^File %))
                                        distinct
                                        (remove @seen))]
                            (.reset k)
                            (when (seq fs)
                              (let [ready (filterv #(settled? (File. ^String %)) fs)]
                                (swap! seen into ready)
                                (when (seq ready) (on-files ready)))))))
                      (catch java.nio.file.ClosedWatchServiceException _ nil)
                      (catch Throwable t (binding [*out* *err*] (println "folder watch stopped:" (.getMessage t))))))
                  "darkroom-watch")
            (.setDaemon true) (.start))]
    (fn stop! []
      (reset! running false)
      (.close ws)
      (.join t 2000))))
