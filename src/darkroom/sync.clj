(ns darkroom.sync
  "Catalog sync over WebDAV (Nextcloud, ownCloud, a NAS, Apache / nginx with
  dav, ...): the catalog (shoots, ratings, labels, keywords, edits, history,
  presets) is stored as `catalog.edn` in a remote folder. Photographs are NOT
  uploaded, and the catalog refers to them by path, so this suits computers where
  the photos sit at the same path, or whose paths a prefix map relates (see
  darkroom.catalog/rewrite-paths).

  Conflicts are detected, never resolved silently: a push carries the ETag of
  the version this computer last pulled or pushed (`If-Match`; the first push
  uses `If-None-Match: *`) and a server holding something newer answers 412,
  which is reported so the user can pull first. A pull replaces the catalog
  after backing up the old one. There is no merging. Pure logic over
  java.net.http, no UI dependency."
  (:require [clojure.string :as str])
  (:import (java.net URI)
           (java.net.http HttpClient HttpClient$Redirect HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers)
           (java.time Duration)
           (java.util Base64)))

(def ^:private timeout (Duration/ofSeconds 30))

(defn- client ^HttpClient []
  (-> (HttpClient/newBuilder) (.followRedirects HttpClient$Redirect/NORMAL) (.connectTimeout timeout) .build))

(defn- basic [user password]
  (str "Basic " (.encodeToString (Base64/getEncoder) (.getBytes (str user ":" password) "UTF-8"))))

(defn- uri-of ^URI [^String s]
  (try (let [u (URI/create s)]
         (when-not (#{"http" "https"} (.getScheme u)) (throw (IllegalArgumentException. "the address must start with http:// or https://")))
         u)
       (catch IllegalArgumentException e (throw (ex-info (str "Bad server address: " (.getMessage e)) {:kind :address} e)))))

(defn remote-uri
  "URI of catalog.edn inside the folder `url`."
  ^URI [url]
  (uri-of (str (str/replace (str/trim (str url)) #"/+$" "") "/catalog.edn")))

(defn- folder-uri ^URI [url] (uri-of (str (str/replace (str/trim (str url)) #"/+$" "") "/")))

(defn- request
  ^HttpRequest [method ^URI uri {:keys [user password]} headers ^String body]
  (let [b (-> (HttpRequest/newBuilder uri) (.timeout timeout))]
    (when-not (str/blank? user) (.header b "Authorization" (basic user password)))
    (doseq [[k v] headers] (.header b ^String k ^String v))
    (.method b method (if body (HttpRequest$BodyPublishers/ofString body) (HttpRequest$BodyPublishers/noBody)))
    (.build b)))

(defn- send! [req]
  (try (.send (client) req (HttpResponse$BodyHandlers/ofString))
       (catch java.io.IOException e (throw (ex-info (str "Cannot reach the server: " (.getMessage e)) {:kind :network} e)))
       (catch IllegalArgumentException e (throw (ex-info (str "Bad server address: " (.getMessage e)) {:kind :address} e)))))

(defn- response-etag [^java.net.http.HttpResponse resp] (.orElse (.firstValue (.headers resp) "ETag") nil))

(defn- check-auth! [^java.net.http.HttpResponse resp]
  (case (long (.statusCode resp))
    (401 403) (throw (ex-info "The server refused the user name or password" {:kind :auth}))
    nil))

(defn push!
  "Uploads `text` (the catalog as EDN) to the folder at (:url conn). `conn` {:url
  :user :password :etag}: pass the ETag of the remote version this computer is
  based on (nil for a first push: refuses to overwrite a remote catalog that
  already exists). Creates the folder when it is missing. Returns {:etag e}; throws
  ex-info with :kind :conflict when the remote changed, :auth, :network, :server."
  [{:keys [url etag] :as conn} ^String text]
  (let [uri (remote-uri url)
        put (fn [] (send! (request "PUT" uri conn
                                   {"Content-Type" "application/edn; charset=utf-8"
                                    (if etag "If-Match" "If-None-Match") (or etag "*")}
                                   text)))
        ^java.net.http.HttpResponse resp (let [^java.net.http.HttpResponse r (put)]
               (if (== 404 (.statusCode r)) ; the folder may not exist yet
                 (let [^java.net.http.HttpResponse m (send! (request "MKCOL" (folder-uri url) conn {} nil))]
                   (check-auth! m)
                   (put))
                 r))]
    (check-auth! resp)
    (case (long (.statusCode resp))
      (200 201 204) {:etag (or (response-etag resp)
                               ;; some servers send no ETag on PUT: ask for it
                               (let [^java.net.http.HttpResponse h (send! (request "HEAD" uri conn {} nil))] (response-etag h)))}
      412 (throw (ex-info (if etag "The catalog on the server changed since this computer last synced: pull first"
                              "The server already has a catalog: pull it first") {:kind :conflict}))
      (throw (ex-info (str "The server answered " (.statusCode resp)) {:kind :server :status (.statusCode resp)})))))

(defn pull!
  "Downloads the remote catalog text. Returns {:text :etag}, or nil when the
  server has none yet. Throws ex-info like push!."
  [{:keys [url] :as conn}]
  (let [^java.net.http.HttpResponse resp (send! (request "GET" (remote-uri url) conn {} nil))]
    (check-auth! resp)
    (case (long (.statusCode resp))
      200 {:text (.body resp) :etag (response-etag resp)}
      404 nil
      (throw (ex-info (str "The server answered " (.statusCode resp)) {:kind :server :status (.statusCode resp)})))))
