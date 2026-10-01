(ns darkroom.imaging.pipeline-test
  (:require [clojure.test :refer [deftest is]]
            [darkroom.imaging.pipeline :as pipeline]
            [darkroom.imaging.scene :as scene]))

(defn- grey [v] (scene/image 1 1 (float-array [v v v])))

(deftest render-passes-neutral-settings-through
  (let [i (grey 0.2)]
    (is (identical? i (pipeline/render i pipeline/default-settings)))
    (is (identical? i (pipeline/render i {:contrast 0.0 :aspect "orig"})))))

(deftest render-applies-every-stage
  (let [i (scene/image 8 8 (float-array (repeat 192 0.2)))
        o (pipeline/render i {:exposure 0.2 :contrast 0.2 :saturation -0.3})]
    (is (not= (vec (:data i)) (vec (:data o))))
    (is (= [8 8] [(:width o) (:height o)]))
    (is (= [8 5] (let [c (pipeline/render i {:aspect "16:9"})] [(:width c) (:height c)]))
        "geometry runs: 16:9 crop of a square")))

(deftest default-settings-cover-every-control
  (doseq [k [:exposure :contrast :highlights :shadows :temp :tint :saturation :fade :bw :grain
             :vignette :angle :aspect :flip :curve :denoise]]
    (is (contains? pipeline/default-settings k) (str k))))
