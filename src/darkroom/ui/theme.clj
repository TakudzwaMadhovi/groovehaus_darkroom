(ns darkroom.ui.theme
  "Groovehaus look: bundled fonts, stylesheet, tracked (letter-spaced) text.
  JavaFX CSS has no letter-spacing, so tracked labels insert thin / hair
  spaces between characters."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (javafx.scene.paint Color)
           (javafx.scene.text Font)))

(def ^:private font-files
  ["BebasNeue-Regular.ttf" "CormorantGaramond-Italic.ttf" "CormorantGaramond-Regular.ttf"
   "Rajdhani-Regular.ttf" "Rajdhani-Medium.ttf" "Rajdhani-SemiBold.ttf" "Rajdhani-Bold.ttf"])

(defonce ^:private fonts
  (delay
    (doseq [f font-files]
      (with-open [in (io/input-stream (io/resource (str "fonts/" f)))]
        (Font/loadFont in 12.0)))))

(defn load-fonts!
  "Registers the bundled fonts with JavaFX (idempotent). FX thread or startup."
  [] @fonts)

(defn stylesheet-url [] (str (io/resource "darkroom.css")))

(def ^:private spacing
  {:tight  " "          ; ~0.1em  (tracking .12-.14em)
   :normal " "          ; ~0.2em  (tracking .18-.24em)
   :wide   "  "})  ; ~0.3em

(defn tracked
  "Letter-spaces `s`. level: :tight, :normal (default) or :wide."
  ([s] (tracked s :normal))
  ([s level]
   (str/join (spacing level) (map str (str s)))))

;; Colours for canvas drawing (mirror the CSS tokens).
(def bone  (Color/web "#F2E9D5"))
(defn bone-a [a] (Color/color 0.949 0.914 0.835 (double a)))
(def sun   (Color/web "#E9B547"))
(def black Color/BLACK)
(def ground (Color/web "#0A0A0A"))
