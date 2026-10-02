(ns groovehaus.looks.core
  "Non-destructive Looks pipeline.

  The source image is never modified. A *stack* is a vector of look configs (see
  groovehaus.looks.registry); rendering decodes the source once, applies each enabled look
  to a private copy, and encodes a fresh BufferedImage. Stacks are plain EDN, so they can be
  stored in a sidecar next to the RAW and replayed at export resolution."
  (:require [clojure.edn :as edn]
            [groovehaus.looks.blend :as blend]
            [groovehaus.looks.buffer :as buffer]
            groovehaus.looks.presets
            [groovehaus.looks.registry :as reg])
  (:import [java.awt.image BufferedImage]))

(set! *warn-on-reflection* true)

(defn- apply-normalized [base {:keys [look-id params blend-mode]}]
  (let [t (double (:intensity params))]
    (if (zero? t)
      base
      (let [styled (buffer/copy-buffer base)]
        ((:render (reg/get-look look-id)) styled params)
        (blend/blend! base styled blend-mode t)))))

(defn apply-look
  "Applies one look config to a working buffer, returning a new buffer."
  [base cfg]
  (let [n (reg/normalize-config cfg)]
    (if (:enabled n) (apply-normalized base n) base)))

(defn renderer
  "Returns (fn [stack]) -> BufferedImage for a fixed source. Keeps the intermediate buffer
  after every layer, so editing layer k only re-renders layers k..n (slider drags on the top
  look cost one look, not the whole stack). Holds one full buffer per layer (~12 B/px), so use it
  on preview-sized images. Not thread-safe; use one per editor session."
  [^BufferedImage src]
  (let [base (buffer/->buffer src)
        cache (atom [])]
    (fn render [stack]
      (let [cfgs (filterv :enabled (mapv reg/normalize-config stack))
            prev @cache
            common (count (take-while true? (map = cfgs (map :cfg prev))))
            kept (vec (take common prev))
            layers (reduce (fn [acc cfg]
                             (let [in (if (seq acc) (:buf (peek acc)) base)]
                               (conj acc {:cfg cfg :buf (apply-normalized in cfg)})))
                           kept (drop common cfgs))]
        (reset! cache layers)
        (buffer/->image (if (seq layers) (:buf (peek layers)) base))))))

(defn render-stack
  "One-shot render of a stack over a BufferedImage."
  ^BufferedImage [^BufferedImage src stack]
  ((renderer src) stack))

(defn catalog
  "Look metadata for building UI controls: id, name, default blend mode, param specs."
  []
  (->> (reg/looks)
       (map (fn [l] {:id (:id l) :name (:name l) :blend-mode (:blend-mode l)
                     :params (reg/param-specs l)}))
       (sort-by :id)))

(defn default-config
  "A look config with every param at its default."
  [look-id]
  (select-keys (reg/normalize-config {:look-id look-id}) [:look-id :params :blend-mode]))

(defn stack->edn [stack] (pr-str (mapv reg/normalize-config stack)))

(defn edn->stack [s] (mapv reg/normalize-config (edn/read-string s)))
