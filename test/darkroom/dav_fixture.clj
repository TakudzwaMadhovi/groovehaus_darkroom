(ns darkroom.dav-fixture
  "Test helper: a tiny in-process WebDAV server (GET / PUT / HEAD / MKCOL for one
  folder, Basic authentication, ETags, If-Match / If-None-Match)."
  (:import (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
           (java.net InetSocketAddress)))

(defn start!
  "Returns {:url :stop! :state atom}; state {:body :etag :folder?} is inspectable and settable."
  [{:keys [user password folder-exists? no-put-etag?] :or {user "u" password "p"}}]
  (let [state (atom {:body nil :etag nil :folder? (boolean folder-exists?) :n 0 :hits []})
        server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)
        reply (fn [^HttpExchange ex code ^String body headers]
                (doseq [[k v] headers] (.add (.getResponseHeaders ex) k v))
                (let [bs (.getBytes (or body "") "UTF-8")]
                  (.sendResponseHeaders ex code (if (= "HEAD" (.getRequestMethod ex)) -1 (if (zero? (alength bs)) -1 (alength bs))))
                  (when (and (pos? (alength bs)) (not= "HEAD" (.getRequestMethod ex))) (with-open [o (.getResponseBody ex)] (.write o bs)))
                  (.close ex)))]
    (.createContext server "/dav/"
      (reify HttpHandler
        (handle [_ ex]
          (let [m (.getRequestMethod ex) path (.getPath (.getRequestURI ex))
                auth (.getFirst (.getRequestHeaders ex) "Authorization")
                want (str "Basic " (.encodeToString (java.util.Base64/getEncoder) (.getBytes (str user ":" password) "UTF-8")))
                body (slurp (.getRequestBody ex))
                if-match (.getFirst (.getRequestHeaders ex) "If-Match")
                if-none (.getFirst (.getRequestHeaders ex) "If-None-Match")
                {:keys [etag folder?]} @state]
            (swap! state update :hits conj [m path])
            (cond
              (not= auth want) (reply ex 401 "no" {"WWW-Authenticate" "Basic realm=\"x\""})
              (= m "MKCOL") (do (swap! state assoc :folder? true) (reply ex 201 "" {}))
              (not folder?) (reply ex 404 "" {})
              (= m "PUT") (cond
                            (and if-match (not= if-match etag)) (reply ex 412 "" {})
                            (and (= if-none "*") etag) (reply ex 412 "" {})
                            :else (let [n (:n (swap! state update :n inc)) e (str "\"v" n "\"")]
                                    (swap! state assoc :body body :etag e)
                                    (reply ex (if etag 204 201) "" (if no-put-etag? {} {"ETag" e}))))
              (and (#{"GET" "HEAD"} m) (:body @state)) (reply ex 200 (:body @state) {"ETag" (:etag @state)})
              :else (reply ex 404 "" {}))))))
    (.start server)
    {:url (str "http://127.0.0.1:" (.getPort (.getAddress server)) "/dav/photos")
     :stop! #(.stop server 0) :state state}))
