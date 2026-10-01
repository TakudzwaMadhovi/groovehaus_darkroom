(ns darkroom.imaging.segment-test
  "AI subject mask. The error paths always run; the end-to-end check needs a real
  U2-Net-style ONNX model and a photo of a person or animal, named by the
  environment variables GROOVEHAUS_TEST_ONNX and GROOVEHAUS_TEST_PHOTO (it is
  skipped, and says so, without them: the model is megabytes and not in the repo)."
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.core :as core]
            [darkroom.imaging.local :as local]
            [darkroom.imaging.mask :as mask]
            [darkroom.imaging.scene :as scene])
  (:import (java.io File)))

(deftest bad-models-are-reported
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not found" (mask/check-model! "/nonexistent/model.onnx")))
  (let [f (File/createTempFile "notamodel" ".onnx")]
    (.deleteOnExit f)
    (spit f "this is not an ONNX file")
    (is (thrown? clojure.lang.ExceptionInfo (mask/check-model! (.getPath f))))
    (testing "a layer with a broken model selects nothing instead of failing the render"
      (let [img (scene/image 8 8 (float-array (* 3 64)))
            layer (assoc (local/new-layer :subject-ai 1 {:model (.getPath f)}) :adj {:exposure 1.0})
            m (local/layer-mask img layer)]
        (is (every? zero? m))))))

(deftest ai-layers-are-scene-dependent-and-labelled
  (is (= "AI SUBJECT" (local/type-labels :subject-ai)))
  (is (= {:model "/m.onnx"} (:shape (local/new-layer :subject-ai 3 {:model "/m.onnx"})))))

(deftest real-model-finds-the-subject
  (let [model (System/getenv "GROOVEHAUS_TEST_ONNX") photo (System/getenv "GROOVEHAUS_TEST_PHOTO")]
    (if-not (and model photo (.isFile (File. ^String model)) (.isFile (File. ^String photo)))
      (println "  (skipped: set GROOVEHAUS_TEST_ONNX and GROOVEHAUS_TEST_PHOTO to run the real-model check)")
      (let [img (scene/from-argb (core/load-image photo))
            w (long (:width img)) h (long (:height img))
            ^floats m (mask/subject-ai img {:model model})
            avg (fn [x0 y0 x1 y1] (let [pts (for [y (range (long (* y0 h)) (long (* y1 h)) 8) x (range (long (* x0 w)) (long (* x1 w)) 8)] (double (aget m (+ (* y w) x))))]
                                    (/ (reduce + pts) (count pts))))]
        (is (= (* w h) (alength m)))
        (is (every? #(<= -1e-6 % 1.000001) (take 5000 m)) "a probability plane")
        (is (> (avg 0.35 0.35 0.65 0.75) 0.8) "the subject in the middle is selected")
        (is (< (avg 0.0 0.0 0.15 0.15) 0.2) "the empty corner is not")))))
