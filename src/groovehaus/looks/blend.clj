(ns groovehaus.looks.blend
  "Per-channel blend modes (W3C Compositing and Blending Level 1 formulas).
  `b` is the backdrop (original image), `s` the source (styled result)."
  (:require [groovehaus.looks.buffer :as buf]))

(set! *warn-on-reflection* true)

(def modes
  #{:normal :multiply :screen :overlay :soft-light :hard-light
    :darken :lighten :color-dodge :color-burn :difference :add})

(defn- hard-light ^double [^double b ^double s]
  (if (<= s 0.5)
    (* 2.0 b s)
    (- 1.0 (* 2.0 (- 1.0 b) (- 1.0 s)))))

(defn- soft-light ^double [^double b ^double s]
  (if (<= s 0.5)
    (- b (* (- 1.0 (* 2.0 s)) b (- 1.0 b)))
    (let [d (if (<= b 0.25) (* (+ (* (- (* 16.0 b) 12.0) b) 4.0) b) (Math/sqrt b))]
      (+ b (* (- (* 2.0 s) 1.0) (- d b))))))

(defn blend-channel ^double [mode ^double b ^double s]
  (case mode
    :normal s
    :multiply (* b s)
    :screen (- 1.0 (* (- 1.0 b) (- 1.0 s)))
    :overlay (hard-light s b)
    :hard-light (hard-light b s)
    :soft-light (soft-light b s)
    :darken (Math/min b s)
    :lighten (Math/max b s)
    :add (Math/min 1.0 (+ b s))
    :difference (Math/abs (- b s))
    :color-dodge (cond (<= b 0.0) 0.0 (>= s 1.0) 1.0 :else (Math/min 1.0 (/ b (- 1.0 s))))
    :color-burn (cond (>= b 1.0) 1.0 (<= s 0.0) 0.0 :else (- 1.0 (Math/min 1.0 (/ (- 1.0 b) s))))))

(defn blend!
  "Writes into `styled`'s rgb: backdrop + intensity * (blend(backdrop, styled) - backdrop)."
  [base styled mode ^double intensity]
  (let [{:keys [^long w ^long h]} base
        ^floats b (:rgb base)
        ^floats s (:rgb styled)
        row (* 3 w)]
    (buf/par-rows h
      (fn [^long y]
        (let [start (* y row)]
          (dotimes [k row]
            (let [i (+ start k)
                  bv (double (aget b i))
                  sv (double (aget s i))
                  r (+ bv (* intensity (- (blend-channel mode bv sv) bv)))]
              (aset s i (float (Math/max 0.0 (Math/min 1.0 r)))))))))
    styled))
