(ns darkroom.dev-test
  "The pure parts of the live-reload loop (dev/darkroom/dev.clj)."
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.dev :as dev])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- tmp-dir ^File [] (.toFile (Files/createTempDirectory "darkroom-dev" (into-array FileAttribute []))))

(defn- write-ns! [^File dir n requires]
  (let [f (File. dir (str (clojure.string/replace (name n) "." "_") ".clj"))]
    (spit f (str "(ns " n (when (seq requires) (str " (:require " (clojure.string/join " " (map #(str "[" % "]") requires)) ")")) ")\n"))
    f))

(deftest reload-order-follows-dependencies
  (let [d (tmp-dir)]
    ;; a -> b -> c ;  d -> c ;  e alone ;  everything also requires clojure.string (not a project ns)
    (write-ns! d 'proj.c ['clojure.string])
    (write-ns! d 'proj.b ['proj.c])
    (write-ns! d 'proj.a ['proj.b])
    (write-ns! d 'proj.d ['proj.c])
    (write-ns! d 'proj.e [])
    (let [g (dev/dependency-graph d)
          order (fn [& changed] (dev/reload-order g changed))]
      (testing "a changed leaf reloads everything that depends on it, dependencies first"
        (let [o (order 'proj.c)]
          (is (= #{'proj.c 'proj.b 'proj.a 'proj.d} (set o)))
          (is (< (.indexOf o 'proj.c) (.indexOf o 'proj.b) (.indexOf o 'proj.a)))
          (is (< (.indexOf o 'proj.c) (.indexOf o 'proj.d)))))
      (testing "a top-level namespace reloads alone"
        (is (= ['proj.a] (order 'proj.a)))
        (is (= ['proj.e] (order 'proj.e))))
      (testing "several changes are combined and ordered once"
        (let [o (order 'proj.b 'proj.d)]
          (is (= #{'proj.b 'proj.a 'proj.d} (set o)))
          (is (= (count o) (count (distinct o))))))
      (testing "unknown namespaces are ignored"
        (is (= [] (order 'not.a.project.ns)))))))

(deftest file-to-namespace-map
  (let [d (tmp-dir)
        f (write-ns! d 'proj.x [])]
    (spit (File. d "notes.txt") "not clojure")
    (spit (File. d "no_ns.clj") "(def x 1)")
    (is (= {f 'proj.x} (dev/file->ns d)) "only files with an ns form, only .clj")))

(deftest change-detection
  (let [d (tmp-dir)
        a (write-ns! d 'p.a [])
        b (write-ns! d 'p.b [])
        seen {(.getPath a) (.lastModified a) (.getPath b) (.lastModified b)}]
    (is (empty? (dev/changed-files seen [a b])))
    (.setLastModified a (+ (.lastModified a) 5000))
    (is (= [a] (vec (dev/changed-files seen [a b]))))
    (is (= [a b] (vec (dev/changed-files {} [a b]))) "files never seen count as changed")))
