(ns darkroom.sync-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.catalog :as cat]
            [darkroom.dav-fixture :as dav]
            [darkroom.sync :as sync]))

(defn- kind [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:kind (ex-data e)))))

(deftest push-and-pull-with-conflict-detection
  (let [{:keys [url stop! state]} (dav/start! {})]
    (try
      (let [conn {:url url :user "u" :password "p"}]
        (testing "first push creates the folder and the file, and returns the ETag"
          (let [r (sync/push! conn "{:a 1}")]
            (is (= "\"v1\"" (:etag r)))
            (is (= "{:a 1}" (:body @state)))
            (is (some #{["MKCOL" "/dav/photos/"]} (:hits @state)))))
        (testing "pull returns text and ETag"
          (is (= {:text "{:a 1}" :etag "\"v1\""} (sync/pull! conn))))
        (testing "a push based on the current ETag succeeds"
          (is (= "\"v2\"" (:etag (sync/push! (assoc conn :etag "\"v1\"") "{:a 2}")))))
        (testing "a push based on an old ETag is refused and changes nothing"
          (is (= :conflict (kind #(sync/push! (assoc conn :etag "\"v1\"") "{:a 3}"))))
          (is (= "{:a 2}" (:body @state))))
        (testing "a first push (no ETag) never overwrites an existing remote catalog"
          (is (= :conflict (kind #(sync/push! conn "{:a 9}"))))
          (is (= "{:a 2}" (:body @state))))
        (testing "wrong password"
          (is (= :auth (kind #(sync/pull! (assoc conn :password "nope")))))
          (is (= :auth (kind #(sync/push! (assoc conn :password "nope") "x"))))))
      (finally (stop!)))))

(deftest servers-that-send-no-etag-on-put
  (let [{:keys [url stop!]} (dav/start! {:no-put-etag? true :folder-exists? true})]
    (try (is (= "\"v1\"" (:etag (sync/push! {:url url :user "u" :password "p"} "{}"))) "asked for with HEAD")
         (finally (stop!)))))

(deftest nothing-on-the-server-yet-and-unreachable-servers
  (let [{:keys [url stop! state]} (dav/start! {:folder-exists? true})]
    (try (is (nil? (sync/pull! {:url url :user "u" :password "p"})) "no catalog yet")
         (finally (stop!))))
  (is (= :network (kind #(sync/pull! {:url "http://127.0.0.1:1/dav/x" :user "u" :password "p"}))))
  (is (= :address (kind #(sync/pull! {:url "not a url at all" :user "u" :password "p"})))))

(deftest catalog-text-round-trip-and-path-rewrite
  (let [c (-> cat/empty-catalog
              (cat/add-shoot "S" ["/Users/me/Photos/a.jpg" "/Users/me/Photos/a.jpg#vc1" "/other/b.jpg"]) first
              (cat/set-adj "/Users/me/Photos/a.jpg" :exposure 0.5))
        text (cat/catalog-text c)
        back (cat/parse-text text)]
    (is (= c back))
    (is (nil? (cat/parse-text "{:not :a-catalog}")))
    (is (nil? (cat/parse-text "(((")))
    (let [moved (cat/rewrite-paths c [["/Users/me/Photos" "D:/Photos"]])]
      (is (= ["D:/Photos/a.jpg" "D:/Photos/a.jpg#vc1" "/other/b.jpg"] (:paths (first (:shoots moved)))))
      (is (= 0.5 (:exposure (cat/adj moved "D:/Photos/a.jpg"))))
      (is (= c (cat/rewrite-paths moved [["D:/Photos" "/Users/me/Photos"]])) "and back"))))
