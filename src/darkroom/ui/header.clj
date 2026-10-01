(ns darkroom.ui.header
  "Global header: LIBRARY / DEVELOP tabs, medallion + title, IMPORT and EXPORT."
  (:require [clojure.java.io :as io]
            [darkroom.ui.state :as st]
            [darkroom.ui.theme :as theme]
            [darkroom.ui.widgets :as w])
  (:import (javafx.geometry Insets Pos)
           (javafx.scene.image Image ImageView)
           (javafx.scene.layout ColumnConstraints GridPane HBox Priority)
           (javafx.scene.control Button)))

(set! *warn-on-reflection* false)

(defn- with-hint
  "Pill text with a dim shortcut hint, e.g. IMPORT + ⌘I."
  [^Button b text hint]
  (.setText b "")
  (.setGraphic b (w/hbox 8
                         (doto (w/label (theme/tracked text :normal) "sys")
                           (.setStyle (if (.contains (.getStyleClass b) "fill-sun") "-fx-text-fill: #000;" "")))
                         (doto (w/label (theme/tracked hint :normal) "sys") (.setOpacity 0.5)
                           (.setStyle (if (.contains (.getStyleClass b) "fill-sun") "-fx-text-fill: #000;" "")))))
  (.setContentDisplay b javafx.scene.control.ContentDisplay/GRAPHIC_ONLY)
  b)

(defn create
  "Returns {:node :refresh!}. `on-import` and `on-export` are no-arg callbacks."
  [{:keys [on-import on-export]}]
  (let [lib   (w/button (theme/tracked "LIBRARY" :normal) (fn [] (st/go! :library)) "nav-btn")
        dev   (w/button (theme/tracked "DEVELOP" :normal) (fn [] (st/go! :develop)) "nav-btn")
        logo  (doto (ImageView. (Image. (str (io/resource "logo-medallion-white.png")) 52.0 52.0 true true))
                (.setFitWidth 26.0) (.setFitHeight 26.0) (.setPreserveRatio true))
        title (doto (w/label (theme/tracked "GROOVEHAUS · DARKROOM" :wide) "sys" "sys-13" "bone")
                (.setStyle "-fx-font-size: 13px;"))
        imp   (with-hint (w/pill "IMPORT +" on-import) "IMPORT +" "⌘I")
        exp   (doto (w/pill "EXPORT ↓" on-export "fill-sun" "in-header"))
        grid  (doto (GridPane.) (.setAlignment Pos/CENTER_LEFT) (.setPadding (Insets. 0 48 0 48)))
        left  (doto (w/hbox 28 lib dev) (.setAlignment Pos/CENTER_LEFT))
        mid   (doto (w/hbox 10 logo title) (.setAlignment Pos/CENTER))
        right (doto (w/hbox 10 imp exp) (.setAlignment Pos/CENTER_RIGHT))]
    (with-hint exp "EXPORT ↓" "⌘E")
    (let [c0 (doto (ColumnConstraints.) (.setHgrow Priority/ALWAYS) (.setPercentWidth -1))
          c1 (ColumnConstraints.)
          c2 (doto (ColumnConstraints.) (.setHgrow Priority/ALWAYS) (.setHalignment javafx.geometry.HPos/RIGHT))]
      (.addAll (.getColumnConstraints grid) (into-array ColumnConstraints [c0 c1 c2])))
    (.add grid left 0 0) (.add grid mid 1 0) (.add grid right 2 0)
    (w/classes! grid "header")
    {:node grid
     :grid grid
     :refresh! (fn [s]
                 (w/set-classes! lib "on" (= :library (:view s)))
                 (w/set-classes! dev "on" (= :develop (:view s))))}))
