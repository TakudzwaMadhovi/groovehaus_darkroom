(ns darkroom.ui.develop-ui-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.pipeline :as pipeline]
            [darkroom.ui.develop :as ui]))

(def ^:private specs @#'ui/slider-specs)

(deftest every-slider-controls-a-real-setting
  (doseq [[tab rows] specs
          [k label lo hi step _opts] rows]
    (testing (str tab "/" label)
      (is (contains? pipeline/default-settings k) "the pipeline knows the setting")
      (let [d (double (pipeline/default-settings k))]
        (is (<= (double lo) d (double hi)) "default lies inside the slider range"))
      (is (< (double lo) (double hi)))
      (is (pos? (double step))))))

(deftest each-setting-has-at-most-one-slider
  (let [keys-seen (map first (mapcat val specs))]
    (is (= (count keys-seen) (count (distinct keys-seen))) "no setting has two sliders")))

(deftest tabs-cover-the-new-panels
  (let [tabs (map first @#'ui/tab-labels)]
    (doseq [t [:basic :detail :color :curve :look :crop :presets :history]]
      (is (some #{t} tabs) (str t)))))
