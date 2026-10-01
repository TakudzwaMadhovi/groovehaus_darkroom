(ns darkroom.ui.histogram-view
  "Histogram strip for the Develop panel (design: bone bars, 64 px high, scaled
  to the 250th-lowest bin so a few spikes do not flatten the rest). Shows luma
  by default or, in RGB mode, the three channels added together (so grey
  reads as white)."
  (:require [darkroom.ui.theme :as theme])
  (:import (javafx.scene.canvas Canvas)
           (javafx.scene.effect BlendMode)
           (javafx.scene.layout Pane)
           (javafx.scene.paint Color)))

(def ^:private cw 640.0)
(def ^:private ch 160.0)

(defn- scale-of
  "Reference bin count: the 250th lowest of 256 (i.e. the 6th highest)."
  ^double [^longs bins]
  (let [sorted (sort (vec bins))]
    (double (max 1 (nth sorted (min 249 (dec (count sorted))))))))

(defn- bars! [gc ^longs bins scale fill]
  (let [bw (/ cw 256.0)]
    (.setFill gc fill)
    (dotimes [i 256]
      (let [h (* ch (min 1.0 (/ (aget bins i) scale)))]
        (.fillRect gc (* i bw) (- ch h) (Math/ceil bw) h)))))

(defn draw!
  "Draws `hist` (see darkroom.imaging.histogram/compute) in `mode` (:luma or :rgb)."
  [^Canvas canvas hist mode]
  (let [gc (.getGraphicsContext2D canvas)]
    (.setGlobalBlendMode gc BlendMode/SRC_OVER)
    (.clearRect gc 0 0 cw ch)
    (when hist
      (if (= mode :rgb)
        (let [scale (apply max (map #(scale-of (% hist)) [:r :g :b]))]
          (.setGlobalBlendMode gc BlendMode/ADD)
          (bars! gc (:r hist) scale (Color/rgb 190 40 40))
          (bars! gc (:g hist) scale (Color/rgb 40 160 60))
          (bars! gc (:b hist) scale (Color/rgb 50 90 200)))
        (when-let [bins (:luma hist)]
          (bars! gc bins (scale-of bins) (theme/bone-a 0.8)))))))

(defn create
  "Returns {:node :update! (fn [hist]) :set-mode! (fn [:luma|:rgb])}. The canvas
  is drawn at 640x160 and scaled to the panel width at 64 px high."
  []
  (let [canvas (Canvas. cw ch)
        mode   (atom :luma)
        last-h (atom nil)
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
    {:node box
     :update! (fn [hist] (reset! last-h hist) (draw! canvas hist @mode))
     :set-mode! (fn [m] (when (not= m @mode) (reset! mode m) (draw! canvas @last-h m)))}))
