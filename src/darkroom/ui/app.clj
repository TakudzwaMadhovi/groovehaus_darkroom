(ns darkroom.ui.app
  "Main window: header, Library and Develop views, export overlay, keyboard
  shortcuts. Watches `state/state` and updates whichever parts changed."
  (:require [darkroom.catalog :as cat]
            [darkroom.imaging.browser :as browser]
            [darkroom.plugin :as plugin]
            [darkroom.ui.canvas :as canvas]
            [darkroom.ui.develop :as develop-view]
            [darkroom.ui.export-overlay :as export-overlay]
            [darkroom.ui.header :as header]
            [darkroom.ui.library :as library]
            [darkroom.ui.state :as st]
            [darkroom.ui.theme :as theme]
            [darkroom.ui.widgets :as w])
  (:import (java.io File)
           (javafx.animation PauseTransition)
           (javafx.application Platform)
           (javafx.beans.binding Bindings)
           (javafx.geometry Insets Pos)
           (javafx.scene Node Scene)
           (javafx.scene.control Slider TextInputControl)
           (javafx.scene.input KeyCode KeyEvent)
           (javafx.scene.layout BorderPane Pane Region StackPane)
           (javafx.stage FileChooser FileChooser$ExtensionFilter Screen Stage)
           (javafx.util Duration)))

(set! *warn-on-reflection* false)

(defn- import-dialog! [owner]
  (let [exts (mapv #(str "*." %) (sort (map #(.toLowerCase ^String %) (java.util.Arrays/asList (javax.imageio.ImageIO/getReaderFileSuffixes)))))
        raw  (mapv #(str "*." %) (sort darkroom.imaging.raw/raw-extensions))
        ch   (doto (FileChooser.) (.setTitle "Import images"))]
    (.addAll (.getExtensionFilters ch)
             (into-array FileChooser$ExtensionFilter
                         [(FileChooser$ExtensionFilter. "Images" ^java.util.List (vec (distinct (concat exts raw))))]))
    (when-let [files (.showOpenMultipleDialog ch owner)]
      (let [n (st/import-paths! (st/image-paths files))]
        (st/toast! (if (pos? (or n 0)) (str n " FRAME" (when (not= n 1) "S") " IMPORTED") "NO NEW IMAGES"))))))

(defn- clamp-width!
  "Binds `node`'s preferred width to clamp(lo, pct * scene width, hi)."
  [^Region node ^Scene scene lo pct hi]
  (let [f (fn [] (.setPrefWidth node (max lo (min hi (* pct (.getWidth scene))))))]
    (.addListener (.widthProperty scene) (w/change-listener (fn [_] (f))))
    (f)))

(defn- toast-node []
  (let [lbl   (w/label "" "sys" "sys-12")
        box   (doto (StackPane. (into-array Node [lbl])) (.setMaxSize Region/USE_PREF_SIZE Region/USE_PREF_SIZE) (.setVisible false) (.setMouseTransparent true))
        timer (PauseTransition. (Duration/seconds 3.2))]
    (w/classes! box "toast")
    (.setOnFinished timer (w/handler (fn [_] (.setVisible box false))))
    (StackPane/setAlignment box Pos/BOTTOM_CENTER)
    (StackPane/setMargin box (Insets. 0 0 28 0))
    {:node box
     :show! (fn [msg] (.setText lbl (theme/tracked (str msg) :normal)) (.setVisible box true) (.playFromStart timer))}))

(defn- typing? [^Scene scene]
  (instance? TextInputControl (.getFocusOwner scene)))

(defn- editing?
  "True when a shortcut may edit photos: no text field has focus and the export dialog is closed."
  [^Scene scene s]
  (not (or (typing? scene) (:exporting s))))

(defn- on-slider?
  "True when the focused control uses the arrow keys itself (sliders, the tone
  curve), so they must not also move between frames."
  [^Scene scene]
  (let [o (.getFocusOwner scene)]
    (boolean (or (instance? Slider o)
                 (and o (.get (.getProperties o) "owns-arrows"))))))

(defn- install-shortcuts! [^Scene scene {:keys [import!]}]
  (.addEventFilter
    scene KeyEvent/KEY_PRESSED
    (w/handler
      (fn [^KeyEvent e]
        (let [code (.getCode e) s @st/state]
          (cond
            (and (.isShortcutDown e) (= code KeyCode/E)) (do (when (:cur s) (swap! st/state assoc :exporting true)) (.consume e))
            (and (.isShortcutDown e) (= code KeyCode/I)) (do (import!) (.consume e))
            (and (.isShortcutDown e) (editing? scene s) (= code KeyCode/Z))
            (do (if (.isShiftDown e) (st/redo!) (st/undo!)) (.consume e))
            (and (.isShortcutDown e) (editing? scene s) (= code KeyCode/Y)) (do (st/redo!) (.consume e))
            (and (.isShortcutDown e) (editing? scene s) (= code KeyCode/A) (= :library (:view s)))
            (do (st/select-all!) (.consume e))
            (and (.isShortcutDown e) (.isShiftDown e) (editing? scene s) (= code KeyCode/C))
            (do (when (st/copy-settings!) (st/toast! "SETTINGS COPIED")) (.consume e))
            (and (.isShortcutDown e) (.isShiftDown e) (editing? scene s) (= code KeyCode/V))
            (do (if-let [n (st/paste-settings!)] (st/toast! (str n " PASTED")) (st/toast! "COPY SETTINGS FIRST")) (.consume e))
            (or (.isShortcutDown e) (.isAltDown e)) nil
            (and (= code KeyCode/BACK_SLASH) (= :develop (:view s))) (do (swap! st/state assoc :before true) (.consume e))
            (typing? scene) nil
            (= code KeyCode/ESCAPE) (do (swap! st/state assoc :exporting false :pick nil :survey false) (st/clear-selection!))
            (:exporting s) nil
            (and (#{KeyCode/UP KeyCode/DOWN} code) (= :library (:view s)) (not (on-slider? scene)))
            (do (st/move! (* (if (= code KeyCode/DOWN) 1 -1) (long @library/grid-columns))) (.consume e))
            (and (= code KeyCode/RIGHT) (not (on-slider? scene)))
            (do (if (= :library (:view s)) (st/move! 1) (st/nav! 1)) (.consume e))
            (and (= code KeyCode/LEFT) (not (on-slider? scene)))
            (do (if (= :library (:view s)) (st/move! -1) (st/nav! -1)) (.consume e))
            (and (= code KeyCode/ENTER) (= :library (:view s))
                 (not (some-> (.getFocusOwner scene) .getStyleClass (.contains "gh-btn"))))
            (do (st/go! :develop) (.consume e))
            :else
            (let [t (.getText e)]
              (cond
                (re-matches #"[0-5]" t) (st/rate! (Long/parseLong t))
                (= t "g") (st/go! :library)
                (= t "d") (st/go! :develop)
                (= t "j") (when (= :develop (:view s)) (swap! st/state update :clip-view not))
                (= t "x") (st/reject!)
                (= t "n") (when (= :library (:view s)) (swap! st/state update :survey not))
                (re-matches #"[6-9]" t) (st/label! (nth cat/colour-labels (- (Long/parseLong t) 6)))
                (= t "e") (when (:cur s) (swap! st/state assoc :exporting true)))))))))
  (.addEventFilter
    scene KeyEvent/KEY_RELEASED
    (w/handler
      (fn [^KeyEvent e]
        (when (and (= (.getCode e) KeyCode/BACK_SLASH) (= :develop (:view @st/state)))
          (swap! st/state assoc :before false))))))

(defn build-scene
  "Builds the window contents. Returns {:scene :stage-title}. Must run on the FX thread."
  []
  (theme/load-fonts!)
  (let [import!   (atom nil)
        library   (library/create {:on-import (fn [] (@import!))})
        develop   (develop-view/create)
        canvas    (canvas/create (:view develop)
                                 {:on-histogram (:update-histogram! develop)
                                  :on-loading   (:set-loading! develop)
                                  :on-error     (fn [path t] (st/toast! (str "CANNOT OPEN " (cat/frame-name path))))})
        export    (export-overlay/create)
        header    (header/create {:on-import (fn [] (@import!))
                                  :on-export (fn [] (when (:cur @st/state) (swap! st/state assoc :exporting true)))})
        toast     (toast-node)
        stack     (StackPane.)
        prev-focus (atom nil)
        root      (doto (BorderPane.) (.setTop (:node header)) (.setCenter stack))
        scene     (Scene. root 1440 900)]
    (.add (.getStylesheets scene) (theme/stylesheet-url))
    (reset! import! #(import-dialog! (.getWindow scene)))
    (w/add! stack (:node library) (:node develop) (:node export) (:node toast))
    (.setMinWidth root 900.0)
    ;; fluid widths: clamp(min, pct of window, max)
    (clamp-width! (:aside library) scene 220 0.19 280)
    (clamp-width! (:panel develop) scene 300 0.26 380)
    (.addListener (.widthProperty scene)
                  (w/change-listener
                    (fn [v]
                      (let [wd (double v)]
                        (.setPadding (:grid header) (Insets. 0 (max 16 (min 48 (* 0.03 wd))) 0 (max 16 (min 48 (* 0.03 wd)))))
                        (.setPadding (:node develop) (Insets. (max 12 (min 24 (* 0.016 wd))) (max 12 (min 40 (* 0.024 wd)))
                                                              (max 12 (min 24 (* 0.016 wd))) (max 12 (min 40 (* 0.024 wd)))))))))
    (install-shortcuts! scene {:import! (fn [] (@import!))})
    ;; focus-visible: rings only while the keyboard is in use
    (.addEventFilter scene KeyEvent/KEY_PRESSED
                     (w/handler (fn [_] (w/set-classes! root "kbd" true))))
    (.addEventFilter scene javafx.scene.input.MouseEvent/MOUSE_PRESSED
                     (w/handler (fn [_] (w/set-classes! root "kbd" false))))
    (let [refresh!
          (fn [old s]
            (let [lib? (= :library (:view s)) dev? (= :develop (:view s))]
              (.setVisible (:node library) lib?) (.setManaged (:node library) lib?)
              (.setVisible (:node develop) dev?) (.setManaged (:node develop) dev?)
              ((:refresh! header) s)
              ((:refresh! export) s)
              ;; modal export: nothing behind it can take focus; focus returns afterwards
              (when (not= (boolean (:exporting old)) (boolean (:exporting s)))
                (let [open? (boolean (:exporting s))]
                  (doseq [^Node n [(:node header) (:node library) (:node develop)]] (.setDisable n open?))
                  (if open?
                    (do (reset! prev-focus (.getFocusOwner scene))
                        (Platform/runLater #((:focus-first! export))))
                    (Platform/runLater #(let [^Node f @prev-focus]
                                          (when (and f (.getScene f) (not (.isDisabled f))) (.requestFocus f)))))))
              (when lib? ((:refresh! library) s))
              (when dev?  ((:refresh! develop) s))
              ;; library thumbnails size
              (when (not= (:tsz old) (:tsz s)) ((:relayout! library)))
              ;; toast
              (when (and (:toast s) (not= (:toast old) (:toast s))) ((:show! toast) (:msg (:toast s))))
              ;; rendering triggers (Develop only)
              (when dev?
                (let [path (:cur s)]
                  (cond
                    (or (not= :develop (:view old)) (not= path (:cur old)) (not= (:before old) (:before s))
                        (not= (= :crop (:tab old)) (= :crop (:tab s)))
                        (not= (boolean (:clip-view old)) (boolean (:clip-view s)))
                        (not= (boolean (:local-mask old)) (boolean (:local-mask s)))
                        (and (:local-mask s) (not= (:local-sel old) (:local-sel s)))
                        (not= (= :local (:tab old)) (= :local (:tab s))))
                    ((:request! canvas) s :preview)
                    (and path (not= (cat/adj (:catalog old) path) (cat/adj (:catalog s) path)))
                    ((:request! canvas) s :draft))))))]
      (add-watch st/state ::ui (fn [_ _ old s] (Platform/runLater #(refresh! old s))))
      (refresh! {} @st/state))
    scene))

(defonce ^:private stage* (atom nil))

(defn rebuild!
  "Development aid: rebuilds the whole window from the current state, keeping the
  window's size and position and all application state. Used after code or
  stylesheet reloads (see dev/darkroom/dev.clj). Safe to call from any thread."
  []
  (Platform/runLater
    (fn []
      (when-let [^Stage stage @stage*]
        (let [w (.getWidth stage) h (.getHeight stage) x (.getX stage) y (.getY stage)]
          (.setScene stage (build-scene))
          (.setX stage x) (.setY stage y) (.setWidth stage w) (.setHeight stage h))))))

(defn show!
  "Starts the toolkit if needed and shows the main window."
  [{:keys [file]}]
  (let [open! (fn []
                (let [_     (plugin/load-dir! (plugin/plugin-dir (cat/app-dir)))
                      scene (build-scene)
                      stage (doto (Stage.) (.setTitle "Groovehaus Darkroom") (.setScene scene)
                              (.setMinWidth 900) (.setMinHeight 620))]
                  (reset! stage* stage)
                  (.setOnCloseRequest stage (w/handler (fn [_] (st/save-now!))))
                  (st/load-catalog!)
                  (when file (st/open-file! file))
                  (.show stage)
                  (when-let [{plugin-file :file message :message} (first (plugin/errors))]
                    (st/toast! (str "PLUGIN " (.toUpperCase ^String plugin-file) " FAILED — " (.toUpperCase ^String message))))))]
    (try
      (Platform/startup ^Runnable open!)
      (catch IllegalStateException _ (Platform/runLater open!)))))
