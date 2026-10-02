(ns groovehaus.looks.presets
  "Built-in looks. Each :render fn mutates the private buffer it is given; blending against
  the original at :intensity / :blend-mode is done by the pipeline, not here."
  (:require [groovehaus.looks.ops :as ops]
            [groovehaus.looks.registry :refer [register-look!]]))

(def ^:private grain-params
  {:grain-amount {:type :unit :default 0.2}
   :grain-size {:type :number :min 0.25 :max 4.0 :default 1.0}
   :seed {:type :int :default 1}})

(register-look!
 {:id :vintage-bw
  :name "Vintage B&W"
  :params (merge grain-params
                 {:vignette-radius {:type :unit :default 0.6}
                  :vignette-strength {:type :unit :default 0.6}
                  :fade {:type :unit :default 0.1}
                  :contrast {:type :unit :default 0.3}})
  :render (fn [buf p]
            (ops/grayscale! buf)
            (ops/contrast! buf (:contrast p))
            (ops/fade! buf (:fade p))
            (ops/vignette! buf (:vignette-radius p) (:vignette-strength p))
            (ops/grain! buf {:amount (:grain-amount p) :size (:grain-size p)
                             :chroma 0.0 :seed (:seed p)}))})

(register-look!
 {:id :film-grain
  :name "Film Grain"
  :params (merge grain-params
                 {:grain-amount {:type :unit :default 0.35}
                  :grain-size {:type :number :min 0.25 :max 4.0 :default 1.2}
                  :chroma {:type :unit :default 0.25}
                  :fade {:type :unit :default 0.05}
                  :contrast {:type :unit :default 0.15}})
  :render (fn [buf p]
            (ops/contrast! buf (:contrast p))
            (ops/fade! buf (:fade p))
            (ops/grain! buf {:amount (:grain-amount p) :size (:grain-size p)
                             :chroma (:chroma p) :seed (:seed p)}))})

(register-look!
 {:id :light-leaks
  :name "Light Leaks"
  :params {:position {:type :enum :values (set (keys ops/leak-positions)) :default :top-right}
           :size {:type :number :min 0.1 :max 2.0 :default 0.7}
           :color {:type :color :default [1.0 0.45 0.12]}
           :seed {:type :int :default 1}}
  :render (fn [buf p]
            (ops/light-leak! buf (:position p) (:size p) (:color p) (:seed p)))})

(register-look!
 {:id :cross-process
  :name "Cross-Processing"
  :params {:fade {:type :unit :default 0.05}
           :contrast {:type :unit :default 0.15}}
  ;; Emulates E-6 film in C-41 chemistry: steep red, lifted/compressed blue, mild green.
  :render (fn [buf p]
            (ops/curves! buf
                         (fn [v] (ops/s-curve v 1.0))
                         (fn [v] (+ 0.02 (* 0.98 (ops/s-curve v 0.6))))
                         (fn [v] (+ 0.12 (* 0.76 (ops/s-curve v -0.35)))))
            (ops/contrast! buf (:contrast p))
            (ops/fade! buf (:fade p)))})

(register-look!
 {:id :duotone
  :name "Duotone"
  :params {:shadow {:type :color :default [0.10 0.04 0.30]}
           :highlight {:type :color :default [1.0 0.62 0.35]}
           :contrast {:type :unit :default 0.2}}
  :render (fn [buf p]
            (ops/grayscale! buf)
            (ops/contrast! buf (:contrast p))
            (ops/duotone! buf (:shadow p) (:highlight p)))})
