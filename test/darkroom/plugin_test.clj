(ns darkroom.plugin-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [darkroom.imaging.pipeline :as pipeline]
            [darkroom.imaging.scene :as scene]
            [darkroom.plugin :as plugin])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(use-fixtures :each (fn [t] (plugin/reset-for-tests!) (try (t) (finally (plugin/reset-for-tests!)))))

(defn- tmp ^File [] (.toFile (Files/createTempDirectory "darkroom-plugins" (into-array FileAttribute []))))

(defn- grey [v] (scene/image 2 2 (float-array (repeat 12 (float v)))))

(deftest example-plugin-loads-and-renders
  (let [dir (tmp)]
    (Files/copy (.toPath (File. "examples/plugins/warm_glow.clj")) (.toPath (File. dir "warm_glow.clj"))
                (into-array java.nio.file.CopyOption []))
    (is (= 1 (plugin/load-dir! dir)))
    (is (= [] (plugin/errors)))
    (let [[f] (plugin/filters-list)]
      (is (= [:warm-glow "WARM GLOW"] ((juxt :id :name) f)))
      (is (= [{:key :amount :label "AMOUNT" :min 0 :max 1 :step 0.01 :default 0.0}] (:params f))))
    (testing "default parameters: the stage is skipped and the image is the very same object"
      (let [img (grey 0.5)]
        (is (identical? img (pipeline/render img pipeline/default-settings)))
        (is (identical? img (pipeline/render img {:plugins {:warm-glow {:amount 0.0}}})))))
    (testing "away from the default the plugin runs, through the normal pipeline"
      (let [out (pipeline/render (grey 0.5) {:plugins {:warm-glow {:amount 1.0}}})
            ^floats d (:data out)]
        (is (> (aget d 0) 0.5)) (is (< (aget d 2) 0.5))))
    (testing "the stage is cached like any other and reacts to a change of the parameter"
      (let [r (pipeline/renderer (grey 0.5))
            a (r {:plugins {:warm-glow {:amount 0.5}}}) b (r {:plugins {:warm-glow {:amount 1.0}}})]
        (is (< (aget ^floats (:data a) 0) (aget ^floats (:data b) 0)))))))

(deftest hooks-run-and-faults-are-contained
  (let [seen (atom [])]
    (plugin/on! :after-export (fn [x] (swap! seen conj [:a x])))
    (plugin/on! :after-export (fn [_] (throw (ex-info "plugin bug" {}))))
    (plugin/on! :after-export (fn [x] (swap! seen conj [:c x])))
    (plugin/run-hooks! :after-export {:file "x.jpg"})
    (is (= [[:a {:file "x.jpg"}] [:c {:file "x.jpg"}]] @seen) "a failing hook does not stop the others"))
  (testing "a failing filter leaves the picture as it was"
    (plugin/register-filter! {:id :bad :name "BAD" :params [{:key :k :label "K" :min 0 :max 1 :default 0}]
                              :run (fn [_ _ _] (throw (ex-info "boom" {})))})
    (let [img (grey 0.3)]
      (is (identical? img (pipeline/render img {:plugins {:bad {:k 1}}}))
          "the stage ran but handed back the input"))))

(deftest broken-plugin-files-are-reported-not-fatal
  (let [dir (tmp)]
    (spit (File. dir "a_good.clj") "(require 'darkroom.plugin) (darkroom.plugin/register-filter! {:id :ok :name \"OK\" :params [] :run (fn [i _ _] i)})")
    (spit (File. dir "b_syntax.clj") "(this is not (closed")
    (spit (File. dir "c_invalid.clj") "(require 'darkroom.plugin) (darkroom.plugin/register-filter! {:id :nope :name \"NOPE\"})")
    (spit (File. dir "notes.txt") "ignored")
    (is (= 1 (plugin/load-dir! dir)))
    (is (= [:ok] (mapv :id (plugin/filters-list))))
    (is (= ["b_syntax.clj" "c_invalid.clj"] (mapv :file (plugin/errors))))
    (is (re-find #"run" (:message (second (plugin/errors)))))
    (is (= 0 (plugin/load-dir! (File. "/nonexistent/plugins"))))))

(deftest registration-is-validated
  (is (thrown? clojure.lang.ExceptionInfo (plugin/register-filter! {:id "str" :name "X" :run identity})))
  (is (thrown? clojure.lang.ExceptionInfo (plugin/register-filter! {:id :x :name "" :run identity})))
  (is (thrown? clojure.lang.ExceptionInfo (plugin/register-filter! {:id :x :name "X" :run identity
                                                                    :params [{:key :a :label "A" :min 0 :max 1 :default 5}]})))
  (testing "registering an id again replaces it but keeps its place"
    (plugin/register-filter! {:id :one :name "ONE" :run identity})
    (plugin/register-filter! {:id :two :name "TWO" :run identity})
    (plugin/register-filter! {:id :one :name "ONE v2" :run identity})
    (is (= ["ONE v2" "TWO"] (mapv :name (plugin/filters-list))))))
