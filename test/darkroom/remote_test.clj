(ns darkroom.remote-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.remote :as remote])
  (:import (java.net URI)
           (java.net.http HttpClient HttpClient$Redirect HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers)))

(def ^:private jpeg (byte-array [-1 -40 -1 -39]))

(defn- backend []
  (let [frames (atom {"s1" [{:id "f1" :name "IMG_1" :rating 0 :colour nil :reject false :edited false}
                            {:id "f2" :name "IMG \"2\"" :rating 3 :colour "red" :reject false :edited true}]})]
    {:shoots (fn [] [{:id "s1" :name "TRIP / été" :count 2}])
     :frames (fn [sid] (get @frames sid))
     :thumb (fn [fid] (when (#{"f1" "f2"} fid) jpeg))
     :preview (fn [fid] (when (= fid "f1") jpeg))
     :update (fn [fid change]
               (let [upd (fn [f] (if (= fid (:id f)) (merge f (update change :colour #(some-> % name))) f))]
                 (when (#{"f1" "f2"} fid)
                   (swap! frames update "s1" #(mapv upd %))
                   (first (filter #(= fid (:id %)) (get @frames "s1"))))))
     :frames-atom frames}))

(defn- http [method url & {:keys [cookie body headers]}]
  (let [c (-> (HttpClient/newBuilder) (.followRedirects HttpClient$Redirect/NEVER) .build)
        b (HttpRequest/newBuilder (URI/create url))]
    (when cookie (.header b "Cookie" cookie))
    (doseq [[k v] headers] (.header b k v))
    (.method b method (if body (HttpRequest$BodyPublishers/ofString body) (HttpRequest$BodyPublishers/noBody)))
    (let [r (.send c (.build b) (HttpResponse$BodyHandlers/ofByteArray))]
      {:status (.statusCode r) :body (.body r) :headers (.map (.headers r))})))

(defn- text [r] (String. ^bytes (:body r) "UTF-8"))

(deftest json-encoding
  (is (= "{\"a\":[1,2.5,true,null,\"x\\\"y\\n\"],\"b\":{\"c\":\"d\"}}" (remote/json {:a [1 2.5 true nil "x\"y\n"] "b" {:c :d}})))
  (is (= "\"\\u0001\"" (remote/json "\u0001")) "control characters are escaped")
  (is (= 32 (count (remote/new-token))))
  (is (not= (remote/new-token) (remote/new-token))))

(deftest the-companion-server
  (let [b (backend)
        {:keys [url port token stop!]} (remote/start! b {:port 0 :host "127.0.0.1" :token "sekret"})
        base (str "http://127.0.0.1:" port)]
    (try
      (is (= (str base "/?t=sekret") url))
      (testing "everything needs the token"
        (is (= 403 (:status (http "GET" (str base "/")))))
        (is (= 403 (:status (http "GET" (str base "/api/shoots")))))
        (is (= 403 (:status (http "GET" (str base "/api/shoots?t=wrong")))))
        (is (= 403 (:status (http "GET" (str base "/api/shoots") :cookie "gh=wrong")))))
      (testing "the first visit sets a cookie and redirects away from the token"
        (let [r (http "GET" (str base "/?t=sekret"))]
          (is (= 302 (:status r)))
          (is (= ["/"] (get (:headers r) "Location")))
          (is (re-find #"gh=sekret; Path=/; HttpOnly; SameSite=Strict" (first (get (:headers r) "Set-Cookie"))))))
      (let [ck "gh=sekret"]
        (testing "the page"
          (let [r (http "GET" (str base "/") :cookie ck)]
            (is (= 200 (:status r)))
            (is (re-find #"Groovehaus Darkroom" (text r)))))
        (testing "shoots and frames as JSON, without file paths"
          (let [r (http "GET" (str base "/api/shoots") :cookie ck)]
            (is (= "[{\"id\":\"s1\",\"name\":\"TRIP / été\",\"count\":2}]" (text r))))
          (let [t (text (http "GET" (str base "/api/shoot/s1") :cookie ck))]
            (is (re-find #"\"name\":\"IMG_1\"" t)) (is (re-find #"IMG \\\"2\\\"" t)))
          (is (= 404 (:status (http "GET" (str base "/api/shoot/none") :cookie ck)))))
        (testing "images"
          (let [r (http "GET" (str base "/api/thumb/f1") :cookie ck)]
            (is (= 200 (:status r))) (is (= ["image/jpeg"] (get (:headers r) "Content-Type"))) (is (= (vec jpeg) (vec (:body r)))))
          (is (= 200 (:status (http "GET" (str base "/api/preview/f1") :cookie ck))))
          (is (= 404 (:status (http "GET" (str base "/api/preview/f2") :cookie ck))))
          (is (= 404 (:status (http "GET" (str base "/api/thumb/zzz") :cookie ck)))))
        (testing "rating, reject and colour can be changed, nothing else"
          (let [post (fn [fid body & {:keys [hdr] :or {hdr {"X-Requested-With" "gh"}}}]
                       (http "POST" (str base "/api/frame/" fid) :cookie ck :body body :headers hdr))
                r (post "f1" "rating=4&colour=blue")]
            (is (= 200 (:status r)))
            (is (re-find #"\"rating\":4" (text r))) (is (re-find #"\"colour\":\"blue\"" (text r)))
            (is (re-find #"\"reject\":true" (text (post "f1" "reject=1"))))
            (is (re-find #"\"colour\":null" (text (post "f1" "colour="))) "an empty colour clears it")
            (is (re-find #"\"rating\":5" (text (post "f1" "rating=99"))) "ratings are clamped")
            (is (= 400 (:status (post "f1" "rating=abc"))))
            (is (= 400 (:status (post "f1" "rating=1" :hdr {}))) "a cross-site form post cannot set it: needs the custom header")
            (is (= 404 (:status (post "nope" "rating=1"))))
            (is (= 413 (:status (post "f1" (str "colour=" (apply str (repeat 5000 "x")))))))
            (is (= 405 (or (when (= 404 (:status (http "DELETE" (str base "/api/frame/f1") :cookie ck))) 405) 0)) "other verbs reach nothing")))
        (testing "unknown paths"
          (is (= 404 (:status (http "GET" (str base "/etc/passwd") :cookie ck))))
          (is (= 404 (:status (http "GET" (str base "/api/../secret") :cookie ck))))))
      (finally (stop!)))))

(deftest qr-code-decodes-back-to-the-address
  (let [text "http://192.168.1.20:8765/?t=0123456789abcdef0123456789abcdef"
        {:keys [size on?]} (remote/qr text)
        scale 6 w (* size scale)
        px (int-array (* w w))]
    (dotimes [y w] (dotimes [x w] (aset px (+ (* y w) x) (if (on? (quot x scale) (quot y scale)) (unchecked-int 0xFF000000) (unchecked-int 0xFFFFFFFF)))))
    (let [bmp (com.google.zxing.BinaryBitmap. (com.google.zxing.common.HybridBinarizer. (com.google.zxing.RGBLuminanceSource. w w px)))
          result (.decode (com.google.zxing.qrcode.QRCodeReader.) bmp)]
      (is (= text (.getText result))))))
