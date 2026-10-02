(ns darkroom.imaging.xmp
  "XMP sidecar files: a small RDF/XML document next to a photo that carries its
  rating, colour label, keywords, title, caption, creator and copyright (as
  standard XMP properties, readable by other software) and a lossless copy of
  every edit in Groovehaus Darkroom's own namespace. The basic sliders are also
  written in Adobe Camera Raw's names (crs:) as a best-effort hint for other
  tools; their scales differ, so treat that as approximate: the gdr:Settings
  element is what this app reads back. Pure I/O, no UI dependency."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [darkroom.imaging.paths :as paths])
  (:import (java.io ByteArrayInputStream File)
           (javax.xml.parsers DocumentBuilderFactory)
           (org.w3c.dom Document Element Node NodeList)))

(def ^:private gdr-ns "https://github.com/takudzwamadhovi/groovehaus_darkroom/ns/1.0/")

(defn sidecar-file
  "The .xmp file for a photo: same folder and base name."
  ^File [id]
  (let [f (File. (paths/source-file id))
        n (.getName f)
        base (str/replace n #"\.[^.]+$" "")]
    (File. (.getParentFile f) (str base ".xmp"))))

(defn- esc [s]
  (-> (str s) (str/replace "&" "&amp;") (str/replace "<" "&lt;") (str/replace ">" "&gt;") (str/replace "\"" "&quot;")))

(def ^:private label-names {:red "Red" :yellow "Yellow" :green "Green" :blue "Blue" :purple "Purple"})
(def ^:private label-keys (into {} (map (fn [[k v]] [v k]) label-names)))

(defn- fmt [x] (let [s (format "%.2f" (double x))] (if (neg? (double x)) s (str "+" s))))

(defn- crs-attrs
  "Camera Raw style attributes for the sliders that map cleanly."
  [adj]
  (let [pct (fn [k] (when-let [v (get adj k)] (when-not (zero? (double v)) (fmt (* 100.0 (double v))))))
        num (fn [k] (when-let [v (get adj k)] (when-not (zero? (double v)) (fmt v))))
        crop (:crop adj)]
    (into (sorted-map)
          (remove (comp nil? val)
                  (merge {"crs:Exposure2012" (num :exposure) "crs:Contrast2012" (pct :contrast)
                          "crs:Highlights2012" (pct :highlights) "crs:Shadows2012" (pct :shadows)
                          "crs:Whites2012" (pct :whites) "crs:Blacks2012" (pct :blacks)
                          "crs:Vibrance" (pct :vibrance) "crs:Saturation" (pct :saturation)
                          "crs:Clarity2012" (pct :clarity) "crs:Texture" (pct :texture) "crs:Dehaze" (pct :dehaze)
                          "crs:Sharpness" (when-let [v (:sharpen adj)] (when-not (zero? (double v)) (str (long v))))
                          "crs:LuminanceSmoothing" (when-let [v (:denoise adj)] (when-not (zero? (double v)) (str (long v))))
                          "crs:ColorNoiseReduction" (when-let [v (:denoise-color adj)] (when-not (zero? (double v)) (str (long v))))}
                         (when (and crop (zero? (long (or (:rotate adj) 0))))
                           (let [[x y w h] crop]
                             {"crs:HasCrop" "True"
                              "crs:CropLeft" (format "%.6f" (double x)) "crs:CropTop" (format "%.6f" (double y))
                              "crs:CropRight" (format "%.6f" (+ (double x) (double w))) "crs:CropBottom" (format "%.6f" (+ (double y) (double h)))})))))))

(defn- li-alt [tag text]
  (str "   <" tag "><rdf:Alt><rdf:li xml:lang=\"x-default\">" (esc text) "</rdf:li></rdf:Alt></" tag ">\n"))

(defn ->xmp
  "XMP text for a frame: {:rating :colour :keywords :meta {...} :adj {...}}."
  [{:keys [rating colour keywords meta adj]}]
  (let [attrs (crs-attrs (or adj {}))]
    (str "<?xpacket begin=\"﻿\" id=\"W5M0MpCehiHzreSzNTczkc9d\"?>\n"
         "<x:xmpmeta xmlns:x=\"adobe:ns:meta/\" x:xmptk=\"Groovehaus Darkroom\">\n"
         " <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">\n"
         "  <rdf:Description rdf:about=\"\"\n"
         "    xmlns:xmp=\"http://ns.adobe.com/xap/1.0/\"\n"
         "    xmlns:dc=\"http://purl.org/dc/elements/1.1/\"\n"
         "    xmlns:crs=\"http://ns.adobe.com/camera-raw-settings/1.0/\"\n"
         "    xmlns:gdr=\"" gdr-ns "\"\n"
         "    xmp:CreatorTool=\"Groovehaus Darkroom\"\n"
         "    xmp:Rating=\"" (long (or rating 0)) "\""
         (when colour (str "\n    xmp:Label=\"" (label-names colour) "\""))
         (apply str (for [[k v] attrs] (str "\n    " k "=\"" (esc v) "\"")))
         ">\n"
         (when-let [t (:title meta)] (li-alt "dc:title" t))
         (when-let [t (:caption meta)] (li-alt "dc:description" t))
         (when-let [t (:copyright meta)] (li-alt "dc:rights" t))
         (when-let [t (:creator meta)] (str "   <dc:creator><rdf:Seq><rdf:li>" (esc t) "</rdf:li></rdf:Seq></dc:creator>\n"))
         (when (seq keywords)
           (str "   <dc:subject><rdf:Bag>" (apply str (for [k keywords] (str "<rdf:li>" (esc k) "</rdf:li>"))) "</rdf:Bag></dc:subject>\n"))
         (when adj
           (str "   <gdr:Settings>" (esc (binding [*print-length* nil *print-level* nil] (pr-str adj))) "</gdr:Settings>\n"))
         "  </rdf:Description>\n </rdf:RDF>\n</x:xmpmeta>\n"
         "<?xpacket end=\"w\"?>\n")))

;; ------------------------------------------------------------------ reading

(defn- parse-doc ^Document [^String text]
  (let [f (doto (DocumentBuilderFactory/newInstance)
            (.setNamespaceAware true)
            ;; no DOCTYPE at all: nothing to expand, no external entities
            (.setFeature "http://apache.org/xml/features/disallow-doctype-decl" true)
            (.setXIncludeAware false)
            (.setExpandEntityReferences false))]
    (.parse (.newDocumentBuilder f) (ByteArrayInputStream. (.getBytes text "UTF-8")))))

(defn- children-named [^Element e ns local]
  (let [^NodeList nl (.getElementsByTagNameNS e ns local)]
    (vec (for [i (range (.getLength nl))] (.item nl i)))))

(defn- li-texts [^Element e ns local]
  (vec (for [^Element c (children-named e ns local)
             ^Element li (children-named c "http://www.w3.org/1999/02/22-rdf-syntax-ns#" "li")]
         (.getTextContent li))))

(defn parse-xmp
  "{:rating :colour :keywords :meta :adj} from XMP text, containing only what
  the document has; nil if it cannot be parsed (or contains a DOCTYPE)."
  [text]
  (try
    (let [doc (parse-doc (str/replace (str text) #"^﻿" ""))
          descs (children-named (.getDocumentElement doc) "http://www.w3.org/1999/02/22-rdf-syntax-ns#" "Description")
          ^Element d (first descs)
          xmp "http://ns.adobe.com/xap/1.0/" dc "http://purl.org/dc/elements/1.1/"]
      (when d
        (let [attr (fn [ns local] (let [v (.getAttributeNS d ns local)] (when-not (str/blank? v) v)))
              rating (some-> (attr xmp "Rating") (#(try (Long/parseLong %) (catch Exception _ nil))))
              colour (some-> (attr xmp "Label") label-keys)
              kws (li-texts d dc "subject")
              one (fn [local] (first (li-texts d dc local)))
              meta (into {} (remove (comp nil? val) {:title (one "title") :caption (one "description")
                                                     :copyright (one "rights") :creator (one "creator")}))
              settings (some-> (first (children-named d gdr-ns "Settings")) (.getTextContent) edn/read-string)]
          (into {} (remove (comp nil? val)
                           {:rating (when (and rating (<= 0 rating 5)) rating)
                            :colour colour
                            :keywords (when (seq kws) (vec (sort (distinct (map str/lower-case kws)))))
                            :meta (when (seq meta) meta)
                            :adj (when (map? settings) settings)})))))
    (catch Throwable _ nil)))

;; ----------------------------------------------------------------- files

(defn write-sidecar!
  "Writes the XMP sidecar for `id` from frame data (see ->xmp), atomically.
  Returns the File."
  [id frame-data]
  (let [f (sidecar-file id)
        tmp (File/createTempFile ".xmp-" ".tmp" (.getParentFile f))]
    (try
      (spit tmp (->xmp frame-data) :encoding "UTF-8")
      (java.nio.file.Files/move (.toPath tmp) (.toPath f)
                                (into-array java.nio.file.CopyOption [java.nio.file.StandardCopyOption/REPLACE_EXISTING]))
      f
      (finally (java.nio.file.Files/deleteIfExists (.toPath tmp))))))

(defn read-sidecar
  "Parsed XMP for `id` (see parse-xmp), or nil when there is no readable sidecar."
  [id]
  (let [f (sidecar-file id)]
    (when (.isFile f) (parse-xmp (slurp f :encoding "UTF-8")))))
