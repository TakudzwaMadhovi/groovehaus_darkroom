;; Example plugin for Groovehaus Darkroom. Copy this file into the `plugins` folder
;; next to the catalog (see the README) and restart the editor: a WARM GLOW slider
;; appears on the PLUGINS tab of Develop.
(ns warm-glow
  (:require [darkroom.plugin :as plugin]))

(defn- warm
  "Pushes the picture toward amber: more red, less blue, weighted toward the highlights."
  [{:keys [width height data] :as img} {:keys [amount]} _opts]
  (let [^floats src data
        ^floats out (float-array (alength src))
        a (double amount)]
    (dotimes [i (* (long width) (long height))]
      (let [j (* 3 i)
            r (aget src j) g (aget src (+ j 1)) b (aget src (+ j 2))
            lum (/ (+ r g b) 3.0)
            k (* a (min 1.0 (* 2.0 lum)))]          ; stronger where it is brighter
        (aset out j       (float (* r (+ 1.0 (* 0.25 k)))))
        (aset out (+ j 1) (float (* g (+ 1.0 (* 0.05 k)))))
        (aset out (+ j 2) (float (* b (- 1.0 (* 0.25 k)))))))
    (assoc img :data out)))

(plugin/register-filter!
  {:id     :warm-glow
   :name   "WARM GLOW"
   :params [{:key :amount :label "AMOUNT" :min 0 :max 1 :step 0.01 :default 0.0}]
   :run    warm})

;; Hooks: runs after every export. (println goes to the terminal the editor was started from.)
(plugin/on! :after-export
            (fn [{:keys [file]}] (println "exported" (str file))))
