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

(deftest cached-image-exposes-stage-outputs
  (let [src (scene/image 20 10 (float-array (repeat 600 0.2)))
        r   (pipeline/renderer src)]
    (is (nil? (pipeline/cached-image r :geometry)) "nothing before the first render")
    (let [out (r {:aspect "1:1" :exposure 1.0})
          geo (pipeline/cached-image r :geometry)
          tone (pipeline/cached-image r :tone)]
      (is (= [10 10] [(:width geo) (:height geo)]) "the geometry stage's output is the cropped source")
      (is (identical? out tone) "the last stage's output is what render returned")
      (is (< (Math/abs (- 0.2 (aget ^floats (:data geo) 0))) 1e-6) "geometry is before tone: still the source light")
      (is (> (aget ^floats (:data tone) 0) 0.3) "tone has applied the exposure")
      (is (identical? src (pipeline/cached-image r :denoise)) "a skipped stage hands its input through"))
    (is (nil? (pipeline/cached-image (fn [& _] nil) :geometry)) "not a pipeline renderer")))
