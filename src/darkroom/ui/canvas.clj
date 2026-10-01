(ns darkroom.ui.canvas
  "The Develop canvas: loads the current frame, renders it through the pipeline
  off the UI thread (latest request wins), and feeds the image and histogram to
  the views. Slider moves render at :draft quality; 300 ms after the last
  change a :preview pass refines it."
  (:require [darkroom.catalog :as cat]
            [darkroom.imaging.geometry :as geometry]
            [darkroom.imaging.histogram :as histogram]
            [darkroom.imaging.loader :as loader]
            [darkroom.imaging.pipeline :as pipeline]
            [darkroom.imaging.scene :as scene]
            [darkroom.ui.fx :as fx]
            [darkroom.ui.state :as st])
  (:import (java.util.concurrent ExecutorService)
           (java.util.concurrent.atomic AtomicLong)
           (javafx.animation PauseTransition)
           (javafx.application Platform)
           (javafx.scene.image ImageView)
           (javafx.stage Screen)
           (javafx.util Duration)))

(defonce ^:private ^ExecutorService render-worker (fx/daemon-executor "darkroom-render" 1))
(defonce ^:private ^ExecutorService load-worker (fx/daemon-executor "darkroom-open" 1))

(defn preview-side
  "Long edge of the on-screen working copy: 2000 px on Retina, else 1400
  (design handoff)."
  []
  (if (> (.getOutputScaleX (Screen/getPrimary)) 1.0) 2000 1400))

;; Loaded frames: path -> {:preview scene image :renderer fn}. Small LRU by insertion order.
(defonce ^:private loaded (atom {:order [] :frames {}}))
(defonce ^:private loading (atom #{}))
(def ^:private max-loaded 4)

(defn- remember! [path sess]
  (swap! loaded
         (fn [{:keys [order frames]}]
           (let [order  (conj (vec (remove #{path} order)) path)
                 drop-n (max 0 (- (count order) max-loaded))
                 gone   (take drop-n order)]
             {:order (vec (drop drop-n order))
              :frames (apply dissoc (assoc frames path sess) gone)}))))

(defn session [path] (get-in @loaded [:frames path]))

(defn frame-aspect
  "Width / height of the frame (the image after the quarter turns, before any
  crop) of the loaded frame `path`, or 1.5 if it is not loaded yet."
  [path rotate]
  (if-let [{:keys [preview]} (session path)]
    (let [[w h] (geometry/frame-size (:width preview) (:height preview) rotate)]
      (/ (double w) (double h)))
    1.5))

(defn render-settings
  "The settings the canvas renders for state `s`: the edits, or the original
  while comparing; while the CROP tab is open the whole frame, so the crop
  rectangle can be dragged over it."
  [s path]
  (let [adj (if (:before s) pipeline/default-settings (cat/adj (:catalog s) path))]
    (if (and (= :crop (:tab s)) (not (:before s)))
      (assoc adj :crop nil :aspect "orig")
      adj)))

(defn create
  "Wires the canvas. `view` is the ImageView to draw into; `on-histogram` gets
  histogram data; `on-loading` gets true/false while a frame is loading.
  Returns {:request! (fn [state kind])} with kind :draft (live, schedules a
  refine) or :preview (immediate), and {:loaded? (fn [path])}."
  [^ImageView view {:keys [on-histogram on-loading on-error]}]
  (let [ticket  (AtomicLong.)
        refine  (PauseTransition. (Duration/millis 300))
        render! (fn [s quality]
                  (let [path (:cur s)]
                    (when-let [sess (and path (session path))]
                      (let [mine     (.incrementAndGet ticket)
                            settings (render-settings s path)
                            current? #(and (= mine (.get ticket)) (= path (:cur @st/state)))]
                        (.execute render-worker
                                  (fn []
                                    (when (current?)
                                      (try
                                        (let [img  (scene/->argb ((:renderer sess) settings {:quality quality :scale (:scale sess)}))
                                              hist (histogram/compute img)
                                              fxi  (fx/->fx-image (if (:clip-view s) (histogram/clipping-overlay img) img))]
                                          (when (current?)
                                            (Platform/runLater #(when (current?)
                                                                  (.setImage view fxi)
                                                                  (when on-histogram (on-histogram hist))))))
                                        (catch Throwable t (.printStackTrace t))))))))))
        load!   (fn load! [path]
                  (when (and path (not (session path)) (not (@loading path)))
                    (swap! loading conj path)
                    (when on-loading (on-loading true))
                    (.execute load-worker
                              (fn []
                                (try
                                  (let [source  (loader/load-scene path)
                                        preview (scene/fit source (preview-side))]
                                    (remember! path {:preview preview :renderer (pipeline/renderer preview)
                                                     :scale (/ (double (:width preview)) (double (:width source)))})
                                    (Platform/runLater
                                      (fn []
                                        (swap! loading disj path)
                                        (when (= path (:cur @st/state))
                                          (when on-loading (on-loading false))
                                          (render! @st/state :preview)))))
                                  (catch Throwable t
                                    (Platform/runLater
                                      (fn []
                                        (swap! loading disj path)
                                        (when on-loading (on-loading false))
                                        (when on-error (on-error path t))))))))))]
    (.setOnFinished refine (reify javafx.event.EventHandler
                             (handle [_ _] (render! @st/state :preview))))
    {:request!
     (fn request! [s kind]
       (load! (:cur s))
       (case kind
         :draft   (do (render! s :draft) (.playFromStart refine))
         :preview (do (.stop refine) (render! s :preview))))
     :loaded? (fn [path] (boolean (session path)))}))
