(ns darkroom.imaging.xmp-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.pipeline :as pipeline]
            [darkroom.imaging.xmp :as xmp])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- tmp-dir [] (.toFile (Files/createTempDirectory "darkroom-xmp" (into-array FileAttribute []))))

(def ^:private frame
  {:rating 4 :colour :blue :keywords ["club" "night"]
   :meta {:title "Opening <night> & \"more\"" :caption "Crowd shot" :creator "A. Person" :copyright "(c) 2024 Me"}
   :adj (assoc pipeline/default-settings :exposure 0.5 :contrast -0.2 :crop [0.1 0.2 0.5 0.4] :hsl (assoc (vec (repeat 8 [0.0 0.0 0.0])) 0 [0.5 0.0 0.0])
               :local [{:id 1 :type :linear :shape {:x0 0.5 :y0 0.2 :x1 0.5 :y1 0.6} :adj {:exposure -1.0}}])})

(deftest sidecar-names
  (is (= "/x/y/IMG_0001.xmp" (.getPath (xmp/sidecar-file "/x/y/IMG_0001.ARW"))))
  (is (= "/x/y/a.b.xmp" (.getPath (xmp/sidecar-file "/x/y/a.b.jpg"))))
  (is (= "/x/y/IMG_0001.xmp" (.getPath (xmp/sidecar-file "/x/y/IMG_0001.ARW#vc3"))) "a virtual copy shares its file's sidecar"))

(deftest lossless-round-trip
  (let [back (xmp/parse-xmp (xmp/->xmp frame))]
    (is (= frame back) "rating, label, keywords, notes and every edit survive, including awkward characters")))

(deftest standard-properties-are-readable-by-others
  (let [text (xmp/->xmp frame)]
    (is (str/includes? text "xmp:Rating=\"4\""))
    (is (str/includes? text "xmp:Label=\"Blue\""))
    (is (str/includes? text "<rdf:li>club</rdf:li>"))
    (is (str/includes? text "&lt;night&gt; &amp; &quot;more&quot;") "escaped, so the document stays well-formed")
    (is (str/includes? text "crs:Exposure2012=\"+0.50\""))
    (is (str/includes? text "crs:Contrast2012=\"-20.00\""))
    (is (str/includes? text "crs:HasCrop=\"True\""))
    (is (str/includes? text "crs:CropRight=\"0.600000\"") "left + width")
    (testing "untouched sliders are not written"
      (is (not (str/includes? text "crs:Vibrance"))))))

(deftest minimal-frames
  (let [text (xmp/->xmp {:rating 0})]
    (is (= {} (dissoc (xmp/parse-xmp text) :rating)) "nothing but what was there")
    (is (not (str/includes? text "xmp:Label")))
    (is (nil? (:keywords (xmp/parse-xmp text))))))

(deftest reads-xmp-from-other-software
  (let [lr "<?xpacket begin='' id='W5M0MpCehiHzreSzNTczkc9d'?>
<x:xmpmeta xmlns:x=\"adobe:ns:meta/\"><rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">
 <rdf:Description rdf:about=\"\" xmlns:xmp=\"http://ns.adobe.com/xap/1.0/\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
   xmlns:crs=\"http://ns.adobe.com/camera-raw-settings/1.0/\" xmp:Rating=\"3\" xmp:Label=\"Green\" crs:Exposure2012=\"+1.00\">
  <dc:subject><rdf:Bag><rdf:li>Wedding</rdf:li><rdf:li>Church</rdf:li></rdf:Bag></dc:subject>
  <dc:title><rdf:Alt><rdf:li xml:lang=\"x-default\">The vows</rdf:li></rdf:Alt></dc:title>
 </rdf:Description></rdf:RDF></x:xmpmeta><?xpacket end=\"w\"?>"
        p (xmp/parse-xmp lr)]
    (is (= 3 (:rating p))) (is (= :green (:colour p))) (is (= ["church" "wedding"] (:keywords p)))
    (is (= {:title "The vows"} (:meta p)))
    (is (nil? (:adj p)) "their develop settings are not translated back")))

(deftest rejects-hostile-or-broken-input
  (is (nil? (xmp/parse-xmp "")))
  (is (nil? (xmp/parse-xmp "not xml <")))
  (is (nil? (xmp/parse-xmp "<a><b></a>")))
  (testing "a DOCTYPE (external entity attack) is refused outright"
    (let [secret (File/createTempFile "secret" ".txt")]
      (spit secret "TOP-SECRET")
      (is (nil? (xmp/parse-xmp (str "<?xml version=\"1.0\"?><!DOCTYPE x [<!ENTITY e SYSTEM \"file://" (.getPath secret) "\">]>"
                                    "<x:xmpmeta xmlns:x=\"adobe:ns:meta/\"><rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">"
                                    "<rdf:Description xmlns:xmp=\"http://ns.adobe.com/xap/1.0/\" xmp:Rating=\"1\"/></rdf:RDF></x:xmpmeta>"))))))
  (testing "settings that are not a map are ignored; a bad rating is dropped"
    (let [p (xmp/parse-xmp (str/replace (xmp/->xmp frame) #"<gdr:Settings>.*</gdr:Settings>" "<gdr:Settings>[1 2 3]</gdr:Settings>"))]
      (is (nil? (:adj p))) (is (= 4 (:rating p))))
    (is (nil? (:rating (xmp/parse-xmp (str/replace (xmp/->xmp frame) "xmp:Rating=\"4\"" "xmp:Rating=\"99\"")))))))

(deftest files
  (let [dir (tmp-dir) photo (str (File. dir "IMG_1.jpg"))]
    (is (nil? (xmp/read-sidecar photo)) "no sidecar yet")
    (let [f (xmp/write-sidecar! photo frame)]
      (is (= "IMG_1.xmp" (.getName f)))
      (is (= frame (xmp/read-sidecar photo)))
      (is (= frame (xmp/read-sidecar (str photo "#vc2"))))
      (is (= ["IMG_1.xmp"] (map #(.getName ^File %) (.listFiles dir))) "no temp files left behind"))
    (testing "overwrites"
      (xmp/write-sidecar! photo {:rating 1})
      (is (= 1 (:rating (xmp/read-sidecar photo)))))))
