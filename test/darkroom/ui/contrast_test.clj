(ns darkroom.ui.contrast-test
  "WCAG AA contrast check for every bone-on-dark text colour in the stylesheet."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(def ^:private bone [242 233 213])

(defn- lin [c] (let [c (/ c 255.0)] (if (<= c 0.03928) (/ c 12.92) (Math/pow (/ (+ c 0.055) 1.055) 2.4))))
(defn- luminance [[r g b]] (+ (* 0.2126 (lin r)) (* 0.7152 (lin g)) (* 0.0722 (lin b))))
(defn- contrast [a b]
  (let [la (luminance a) lb (luminance b)]
    (/ (+ (max la lb) 0.05) (+ (min la lb) 0.05))))
(defn- blend [fg a bg] (mapv #(+ (* %1 a) (* %2 (- 1.0 a))) fg bg))

(def ^:private surfaces
  "Backgrounds text can sit on: header, ground, panel, tile, ash, and the
  lightest overlays (selected tile bar, selected shoot row, hovered row)."
  {"header #000"        [0 0 0]
   "ground #0A0A0A"     [10 10 10]
   "panel #121110"      [18 17 16]
   "tile #161413"       [22 20 19]
   "ash #1C1C1C"        [28 28 28]
   "tile bar (on)"      (blend bone 0.10 [22 20 19])
   "shoot row (on)"     (blend bone 0.08 [18 17 16])
   "row hover"          (blend bone 0.06 [18 17 16])})

(def ^:private css (slurp (io/resource "darkroom.css")))

(defn- bone-text-alphas
  "Alphas of every `-fx-text-fill` / `-fx-prompt-text-fill: rgba(242,233,213,A)` in the CSS."
  [text]
  (map #(Double/parseDouble (second %))
       (re-seq #"-fx-(?:prompt-)?text-fill:\s*rgba\(242,233,213,([0-9.]+)\)" text)))

(deftest all-bone-text-meets-aa
  (let [alphas (distinct (bone-text-alphas css))]
    (is (seq alphas))
    (doseq [a alphas, [name bg] surfaces]
      (is (>= (contrast (blend bone a bg) bg) 4.5)
          (format "bone at %.2f on %s: %.2f:1" a name (contrast (blend bone a bg) bg))))))

(deftest accent-and-inverse-text
  (is (>= (contrast [233 181 71] [18 17 16]) 4.5) "sun on panel")
  (is (>= (contrast [0 0 0] [233 181 71]) 4.5) "black on sun pill")
  (is (>= (contrast [0 0 0] bone) 4.5) "black on bone pill")
  (testing "header shortcut hint on the sun pill (black at 72%)"
    (is (>= (contrast (blend [0 0 0] 0.72 [233 181 71]) [233 181 71]) 4.5))))
