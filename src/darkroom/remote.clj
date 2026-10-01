(ns darkroom.remote
  "Phone companion: a small web server inside the editor so a phone or tablet
  on the same network can browse the library, look at developed previews, and
  rate, reject and colour-label frames, from its browser (no app to install).

  It is opt-in and local: it listens only while switched on, on this machine's
  network address, and every request must carry the secret token in the address
  the editor shows (or the cookie it sets on first use). It speaks plain HTTP, so
  anyone on the same network who can see the address (or the traffic) can use it:
  use it on a network you trust. It never exposes file paths, never serves
  anything but thumbnails and previews of library frames, and the only things it
  can change are rating, reject flag and colour label.

  The server only knows a `backend` map of functions (see `start!`); the editor
  supplies them (darkroom.ui.state/remote-backend). Pure logic over
  com.sun.net.httpserver, no UI dependency."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
           (java.net InetAddress InetSocketAddress NetworkInterface URI URLDecoder)
           (java.security MessageDigest SecureRandom)
           (java.util.concurrent Executors)))

;; -------------------------------------------------------------------- json

(defn- json-str ^String [^String s]
  (let [sb (StringBuilder. "\"")]
    (doseq [c s]
      (case c
        \" (.append sb "\\\"")
        \\ (.append sb "\\\\")
        \newline (.append sb "\\n")
        \return (.append sb "\\r")
        \tab (.append sb "\\t")
        \backspace (.append sb "\\b")
        \formfeed (.append sb "\\f")
        (if (< (int c) 32) (.append sb (format "\\u%04x" (int c))) (.append sb c))))
    (str (.append sb "\""))))

(defn json
  "Compact JSON text of maps (keyword or string keys), vectors/seqs, strings,
  numbers, booleans and nil."
  ^String [x]
  (cond (nil? x) "null"
        (string? x) (json-str x)
        (keyword? x) (json-str (name x))
        (number? x) (str x)
        (boolean? x) (str x)
        (map? x) (str "{" (str/join "," (map (fn [[k v]] (str (json-str (if (keyword? k) (name k) (str k))) ":" (json v))) x)) "}")
        (sequential? x) (str "[" (str/join "," (map json x)) "]")
        :else (json-str (str x))))

;; ----------------------------------------------------------------- helpers

(defn new-token
  "A random 128-bit token as 32 hex characters."
  []
  (let [bs (byte-array 16)]
    (.nextBytes (SecureRandom.) bs)
    (apply str (map #(format "%02x" (bit-and (long %) 0xff)) bs))))

(defn- same? [^String a ^String b]
  (MessageDigest/isEqual (.getBytes (str a) "UTF-8") (.getBytes (str b) "UTF-8")))

(defn- query-params [^URI uri]
  (into {} (for [kv (str/split (str (.getRawQuery uri)) #"&") :when (seq kv)
                 :let [[k v] (str/split kv #"=" 2)]]
             [(URLDecoder/decode ^String k "UTF-8") (URLDecoder/decode (str v) "UTF-8")])))

(defn- form-params [^String body]
  (into {} (for [kv (str/split body #"&") :when (seq kv)
                 :let [[k v] (str/split kv #"=" 2)]]
             [(URLDecoder/decode ^String k "UTF-8") (URLDecoder/decode (str v) "UTF-8")])))

(defn- cookie-token [^HttpExchange ex]
  (some (fn [h] (some (fn [part] (let [[k v] (str/split (str/trim part) #"=" 2)] (when (= k "gh") v))) (str/split h #";")))
        (.get (.getRequestHeaders ex) "Cookie")))

(defn- respond!
  ([ex code ^String content-type ^bytes body] (respond! ex code content-type body {}))
  ([^HttpExchange ex code ^String content-type ^bytes body headers]
   (let [h (.getResponseHeaders ex)]
     (.set h "Content-Type" content-type)
     (.set h "Cache-Control" "no-store")
     (.set h "X-Content-Type-Options" "nosniff")
     (.set h "Referrer-Policy" "no-referrer")
     (doseq [[k v] headers] (.set h ^String k ^String v))
     (.sendResponseHeaders ex (int code) (if (zero? (alength body)) -1 (alength body)))
     (when (pos? (alength body)) (with-open [o (.getResponseBody ex)] (.write o body)))
     (.close ex))))

(defn- text! [ex code s] (respond! ex code "text/plain; charset=utf-8" (.getBytes (str s) "UTF-8")))
(defn- json! [ex x] (respond! ex 200 "application/json; charset=utf-8" (.getBytes (json x) "UTF-8")))

(def ^:private max-body 4096)

(defn qr
  "QR code of `text` as {:size n :on? (fn [x y])} (true = dark module, quiet zone
  of 2 modules included), so the address can be scanned from a phone's camera."
  [^String text]
  (let [hints {com.google.zxing.EncodeHintType/MARGIN 2 com.google.zxing.EncodeHintType/ERROR_CORRECTION com.google.zxing.qrcode.decoder.ErrorCorrectionLevel/M}
        m (.encode (com.google.zxing.qrcode.QRCodeWriter.) text com.google.zxing.BarcodeFormat/QR_CODE 0 0 hints)]
    {:size (.getWidth m) :on? (fn [x y] (.get m (int x) (int y)))}))

(defn lan-address
  "A site-local IPv4 address of this machine (what a phone on the same Wi-Fi would reach), else loopback."
  ^String []
  (or (first (for [^NetworkInterface ni (enumeration-seq (NetworkInterface/getNetworkInterfaces))
                   :when (and (.isUp ni) (not (.isLoopback ni)) (not (.isVirtual ni)))
                   ^InetAddress a (enumeration-seq (.getInetAddresses ni))
                   :when (and (instance? java.net.Inet4Address a) (.isSiteLocalAddress a))]
               (.getHostAddress a)))
      "127.0.0.1"))

;; ------------------------------------------------------------------ server

(defn start!
  "Starts the companion server and returns {:url :port :token :stop!}.
  `backend`: {:shoots (fn [] [{:id :name :count}])
              :frames (fn [shoot-id] [{:id :name :rating :colour :reject :edited}] or nil)
              :thumb  (fn [frame-id] jpeg-bytes or nil)
              :preview (fn [frame-id] jpeg-bytes or nil)
              :update (fn [frame-id {:rating n :reject bool :colour kw-or-nil}] updated frame map or nil)}
  opts: :port (default 8765, 0 = any free one), :token (default random),
  :host (default 0.0.0.0, all interfaces; use \"127.0.0.1\" for this machine only)."
  [backend & [{:keys [port token host] :or {port 8765 host "0.0.0.0"}}]]
  (let [token (or token (new-token))
        ^HttpServer server (try (HttpServer/create (InetSocketAddress. ^String host (int port)) 32)
                                (catch java.net.BindException _ (HttpServer/create (InetSocketAddress. ^String host 0) 32)))
        page (with-open [in (io/input-stream (io/resource "remote/index.html"))] (.readAllBytes in))
        authed? (fn [^HttpExchange ex] (let [t (or (get (query-params (.getRequestURI ex)) "t") (cookie-token ex))] (and t (same? t token))))
        handler (reify HttpHandler
                  (handle [_ ex]
                    (try
                      (let [^HttpExchange ex ex
                            m (.getRequestMethod ex) path (.getPath (.getRequestURI ex))
                            segs (vec (remove str/blank? (str/split path #"/")))
                            q (query-params (.getRequestURI ex))]
                        (cond
                          (not (authed? ex)) (text! ex 403 "Open the address shown in the editor.")
                          ;; first visit with ?t=: set the cookie and drop the token from the address bar
                          (and (= m "GET") (get q "t"))
                          (respond! ex 302 "text/plain" (byte-array 0)
                                    {"Location" "/" "Set-Cookie" (str "gh=" token "; Path=/; HttpOnly; SameSite=Strict")})
                          (and (= m "GET") (= path "/")) (respond! ex 200 "text/html; charset=utf-8" page)
                          (and (= m "GET") (= segs ["api" "shoots"])) (json! ex ((:shoots backend)))
                          (and (= m "GET") (= 3 (count segs)) (= ["api" "shoot"] (subvec segs 0 2)))
                          (if-let [fs ((:frames backend) (segs 2))] (json! ex fs) (text! ex 404 "no such shoot"))
                          (and (= m "GET") (= 3 (count segs)) (#{"thumb" "preview"} (segs 1)) (= "api" (segs 0)))
                          (if-let [^bytes b (((keyword (segs 1)) backend) (segs 2))]
                            (respond! ex 200 "image/jpeg" b)
                            (text! ex 404 "no such frame"))
                          (and (= m "POST") (= 3 (count segs)) (= ["api" "frame"] (subvec segs 0 2)))
                          (cond
                            (not= "gh" (.getFirst (.getRequestHeaders ex) "X-Requested-With")) (text! ex 400 "missing header")
                            :else
                            (let [body (let [bs (.readNBytes (.getRequestBody ex) (inc max-body))]
                                         (when (<= (alength bs) max-body) (String. bs "UTF-8")))]
                              (if-not body
                                (text! ex 413 "too large")
                                (let [p (form-params body)
                                      change (cond-> {}
                                               (contains? p "rating") (assoc :rating (max 0 (min 5 (Long/parseLong (get p "rating")))))
                                               (contains? p "reject") (assoc :reject (= "1" (get p "reject")))
                                               (contains? p "colour") (assoc :colour (let [c (get p "colour")] (when-not (str/blank? c) (keyword c)))))]
                                  (if-let [f ((:update backend) (segs 2) change)] (json! ex f) (text! ex 404 "no such frame"))))))
                          :else (text! ex 404 "not found")))
                      (catch NumberFormatException _ (text! ex 400 "bad value"))
                      (catch Throwable t
                        (try (text! ex 500 "server error") (catch Throwable _ nil))
                        (binding [*out* *err*] (println "phone companion error:" (.getMessage t)))))))]
    (.createContext server "/" handler)
    (.setExecutor server (Executors/newFixedThreadPool 4 (reify java.util.concurrent.ThreadFactory
                                                          (newThread [_ r] (doto (Thread. ^Runnable r "darkroom-remote") (.setDaemon true))))))
    (.start server)
    (let [p (.getPort (.getAddress server))
          addr (if (= host "0.0.0.0") (lan-address) host)]
      {:url (str "http://" addr ":" p "/?t=" token) :port p :token token
       :stop! (fn [] (.stop server 0))})))
