(ns darkroom.ui.export-dialog
  "JavaFX dialog that collects export options: destination folder, file name,
  format and (for JPEG) quality. Returns the choices; writes nothing itself."
  (:require [darkroom.imaging.export :as export])
  (:import (java.io File)
           (javafx.geometry Insets)
           (javafx.scene.control ButtonType ChoiceBox Dialog Label Slider TextField Button)
           (javafx.scene.layout ColumnConstraints GridPane HBox Priority)
           (javafx.stage DirectoryChooser Window)))

(defn- initial-dir ^File [^File dir]
  (if (and dir (.isDirectory dir)) dir (File. (System/getProperty "user.home"))))

(defn show
  "Shows the dialog modally. `defaults` is {:dir File :name str}. Returns
  {:dir File :name str :format kw :quality double} or nil if cancelled."
  [^Window owner {:keys [dir name]}]
  (let [chosen    (atom (initial-dir dir))
        dialog    (doto (Dialog.)
                    (.setTitle "Export Image")
                    (.setHeaderText "Save the current adjusted image at full resolution."))
        _         (when owner (.initOwner dialog owner))
        pane      (.getDialogPane dialog)
        name-f    (TextField. (str name))
        dir-label (doto (Label. (str @chosen)) (.setMaxWidth 320))
        browse    (Button. "Choose…")
        fmt-box   (doto (ChoiceBox.)
                    (-> .getItems (.addAll ^java.util.Collection (mapv #(:label (export/formats %)) [:jpeg :png])))
                    (.setValue "JPEG"))
        q-slider  (doto (Slider. 1 100 (* 100 export/default-quality))
                    (.setShowTickMarks true) (.setShowTickLabels true) (.setMajorTickUnit 25))
        q-label   (Label. (str (long (.getValue q-slider))))
        grid      (doto (GridPane.) (.setHgap 10) (.setVgap 10) (.setPadding (Insets. 10)))
        format-kw #(if (= "PNG" (.getValue fmt-box)) :png :jpeg)]
    (.setOnAction browse
                  (reify javafx.event.EventHandler
                    (handle [_ _]
                      (let [chooser (doto (DirectoryChooser.)
                                      (.setTitle "Choose destination folder")
                                      (.setInitialDirectory @chosen))]
                        (when-let [f (.showDialog chooser (.getWindow (.getScene pane)))]
                          (reset! chosen f)
                          (.setText dir-label (str f)))))))
    ;; PNG is lossless: quality does not apply.
    (.addListener (.valueProperty q-slider)
                  (reify javafx.beans.value.ChangeListener
                    (changed [_ _ _ v] (.setText q-label (str (Math/round (.doubleValue ^Number v)))))))
    (.addListener (.valueProperty fmt-box)
                  (reify javafx.beans.value.ChangeListener
                    (changed [_ _ _ _] (.setDisable q-slider (= :png (format-kw))))))
    (doseq [[row [label node]] (map-indexed vector
                                            [["File name" name-f]
                                             ["Folder"    (doto (HBox. 8.0) (-> .getChildren (.add dir-label)) (-> .getChildren (.add browse)))]
                                             ["Format"    fmt-box]
                                             ["Quality"   (doto (HBox. 8.0) (-> .getChildren (.add q-slider)) (-> .getChildren (.add q-label)))]])]
      (.add grid (Label. ^String label) 0 row)
      (.add grid ^javafx.scene.Node node 1 row))
    (.setContent pane grid)
    (.add (.getButtonTypes pane) ButtonType/OK)
    (.add (.getButtonTypes pane) ButtonType/CANCEL)
    (let [result (.showAndWait dialog)]
      (when (and (.isPresent result) (= ButtonType/OK (.get result)))
        {:dir     @chosen
         :name    (.getText name-f)
         :format  (format-kw)
         :quality (/ (Math/round (.getValue q-slider)) 100.0)}))))
