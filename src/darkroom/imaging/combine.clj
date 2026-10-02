(ns darkroom.imaging.combine
  "Merging several frames of the library into a new one: an HDR merge of an
  exposure bracket (darkroom.imaging.merge) or a panorama (darkroom.imaging.panorama).
  Frames are read unedited (as-shot colour, full resolution); the result is saved
  next to the first frame as a 32-bit float, linear working-space TIFF
  (`<name>-HDR.tif` / `<name>-Pano.tif`, never overwriting) that opens like any
  other frame and keeps everything above 1.0. Pure logic, no UI dependency."
  (:require [darkroom.imaging.exif :as exif]
            [darkroom.imaging.export :as export]
            [darkroom.imaging.loader :as loader]
            [darkroom.imaging.merge :as merge]
            [darkroom.imaging.output :as output]
            [darkroom.imaging.panorama :as panorama]
            [darkroom.imaging.paths :as paths])
  (:import (java.io File)))

(defn- target-dir ^File [first-path]
  (.getParentFile (.getAbsoluteFile (File. (paths/source-file first-path)))))

(defn- base-name [first-path suffix]
  (let [n (.getName (File. (paths/source-file first-path)))
        stem (let [i (.lastIndexOf n ".")] (if (pos? i) (subs n 0 i) n))]
    (str stem "-" suffix)))

(defn- save! [sc first-path suffix]
  (let [dir (target-dir first-path)
        name (output/free-name (.getPath dir) (base-name first-path suffix) :tiff32)]
    (export/save-scene! sc {:dir (.getPath dir) :name name :format :tiff32})))

(defn hdr
  "HDR-merges the frames at `paths` (an exposure bracket of one scene, at least
  two, same size). opts :align? (default true), :progress (fn [step total]).
  Returns {:file File :factors [..] :shifts [..] :source :exif|:estimated}."
  [paths & [{:keys [align? progress] :or {align? true}}]]
  (let [tick (fn [i] (when progress (progress i 3)))
        _ (tick 0)
        frames (mapv (fn [p] {:scene (loader/load-scene p) :tags (exif/read-tags p)}) paths)
        _ (tick 1)
        r (merge/merge-bracket frames {:align? align?})
        _ (tick 2)
        f (save! (:scene r) (first paths) "HDR")]
    (tick 3)
    (assoc (dissoc r :scene) :file f)))

(defn pano
  "Stitches the frames at `paths` into a panorama (see darkroom.imaging.panorama
  for what it can do). opts :progress (fn [done total]). Returns {:file File
  :valid-rect fractions of the result that frames fully cover (or nil)}. Throws
  ex-info naming the problem when frames cannot be joined, or when the OpenCV
  feature modules are not available on this machine."
  [paths & [{:keys [progress]}]]
  (let [imgs (mapv #(loader/load-scene %) paths)
        r (try (panorama/panorama imgs {:progress progress})
               (catch UnsatisfiedLinkError e
                 (throw (ex-info "Panoramas need the GTK 2 libraries on Linux (package libgtk2.0-0)" {} e)))
               (catch NoClassDefFoundError e
                 (throw (ex-info "Panoramas need the GTK 2 libraries on Linux (package libgtk2.0-0)" {} e))))
        f (save! (:scene r) (first paths) "Pano")]
    {:file f :valid-rect (:valid-rect r)}))
