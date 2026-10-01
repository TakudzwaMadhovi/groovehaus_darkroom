(ns darkroom.imaging.pipeline
  "Maps a settings map (e.g. {:brightness 20}) onto image operations.

  To add a feature: write an `(fn [image value])` in darkroom.imaging.core (or
  a new namespace), register it in `operations` with a neutral value, and add a
  control for it in the UI. Operations run in the order listed."
  (:require [darkroom.imaging.core :as core]))

(def operations
  "Ordered [setting-key operation-fn neutral-value] entries."
  [[:brightness core/adjust-brightness 0]])

(def default-settings
  (into {} (map (fn [[k _ neutral]] [k neutral])) operations))

(defn render
  "Applies every non-neutral setting to `source` and returns the result."
  [source settings]
  (reduce (fn [img [k op neutral]]
            (let [v (get settings k neutral)]
              (if (= v neutral) img (op img v))))
          source
          operations))
