(ns darkroom.ui.histogram-view
  "Luma histogram strip for the Develop panel (design: bone bars, 64 px high,
  scaled to the 250th-lowest bin so a few spikes do not flatten the rest)."
  (:require [darkroom.ui.theme :as theme])
  (:import (javafx.scene.canvas Canvas)
           (javafx.scene.layout Pane)))

(def ^:private cw 640.0)
(def ^:private ch 160.0)

(defn- scale-of
  "Reference bin count: the 250th lowest of 256 (i.e. the 6th highest)."
  ^double [^longs bins]
  (let [sorted (sort (vec bins))]
    (double (max 1 (nth sorted (min 249 (dec (count sorted))))))))

(defn draw! [^Canvas canvas hist]
  (let [gc (.getGraphicsContext2D canvas)]
    (.clearRect gc 0 0 cw ch)
    (when-let [^longs bins (:luma hist)]
      (let [scale (scale-of bins)
            bw    (/ cw 256.0)]
        (.setFill gc (theme/bone-a 0.8))
        (dotimes [i 256]
          (let [h (* ch (min 1.0 (/ (aget bins i) scale)))]
            (.fillRect gc (* i bw) (- ch h) (Math/ceil bw) h)))))))

(defn create
  "Returns {:node :update!}. The canvas is drawn at 640x160 and scaled to the
  panel width at 64 px high."
  []
  (let [canvas (Canvas. cw ch)
        box    (proxy [Pane] []
                 (layoutChildren []
                   (let [^Pane this this]
                     (.setScaleX canvas (/ (.getWidth this) cw))
                     (.setScaleY canvas (/ 64.0 ch))
                     (.setLayoutX canvas (/ (- (.getWidth this) cw) 2.0))
                     (.setLayoutY canvas (/ (- 64.0 ch) 2.0)))))]
    (.add (.getChildren ^Pane box) canvas)
    (.setMinHeight ^Pane box 64.0) (.setPrefHeight ^Pane box 64.0) (.setMaxHeight ^Pane box 64.0)
    (.setStyle ^Pane box "-fx-border-color: transparent transparent rgba(242,233,213,0.14) transparent; -fx-border-width: 0 0 1 0;")
    {:node box :update! #(draw! canvas %)}))
