(ns darkroom.ui.develop
  "Develop view: canvas with before/after, histogram, tabbed adjustment panel
  (BASIC, CURVE, LOOK, CROP, PRESETS, HISTORY) and the filmstrip."
  (:require [darkroom.catalog :as cat]
            [darkroom.imaging.crop :as crop]
            [darkroom.imaging.develop :as develop]
            [darkroom.imaging.geometry :as geometry]
            [darkroom.imaging.pipeline :as pipeline]
            [darkroom.ui.canvas :as canvas]
            [darkroom.ui.crop-overlay :as crop-overlay]
            [darkroom.ui.curve :as curve]
            [darkroom.ui.histogram-view :as histogram-view]
            [darkroom.ui.state :as st]
            [darkroom.ui.theme :as theme]
            [darkroom.ui.thumbs :as thumbs]
            [darkroom.ui.widgets :as w])
  (:import (javafx.geometry Insets Pos Rectangle2D)
           (javafx.scene.control Button Label ScrollPane ScrollPane$ScrollBarPolicy)
           (javafx.scene.image Image ImageView)
           (javafx.scene.layout BorderPane FlowPane HBox Pane Priority Region StackPane VBox)
           (javafx.scene.shape Rectangle)))

(set! *warn-on-reflection* false)

(defn- dots [n] (str (apply str (repeat n "●")) (apply str (repeat (- 5 n) "○"))))

;; (key label min max step opts); opts :pct? / :decimals
(def ^:private slider-specs
  {:basic [[:exposure "EXPOSURE" -2 2 0.01 {}]
           [:contrast "CONTRAST" -1 1 0.01 {:pct? true}]
           [:highlights "HIGHLIGHTS" -1 1 0.01 {:pct? true}]
           [:shadows "SHADOWS" -1 1 0.01 {:pct? true}]
           [:whites "WHITES" -1 1 0.01 {:pct? true}]
           [:blacks "BLACKS" -1 1 0.01 {:pct? true}]
           [:temp "TEMPERATURE" -1 1 0.01 {:pct? true}]
           [:tint "TINT" -1 1 0.01 {:pct? true}]
           [:vibrance "VIBRANCE" -1 1 0.01 {:pct? true}]
           [:saturation "SATURATION" -1 1 0.01 {:pct? true}]]
   :detail [[:texture "TEXTURE" -1 1 0.01 {:pct? true}]
            [:clarity "CLARITY" -1 1 0.01 {:pct? true}]
            [:dehaze "DEHAZE" -1 1 0.01 {:pct? true}]
            [:sharpen "SHARPEN" 0 100 1 {:decimals 0}]
            [:sharpen-radius "RADIUS" 0.5 3 0.1 {:decimals 1}]
            [:sharpen-masking "MASKING" 0 1 0.01 {:pct? true}]
            [:denoise "DENOISE" 0 100 1 {:decimals 0}]
            [:denoise-color "COLOUR NOISE" 0 100 1 {:decimals 0}]]
   :grading [[:split-sh-hue "SHADOW HUE" 0 360 1 {:decimals 0}]
             [:split-sh-sat "SHADOW SATURATION" 0 1 0.01 {:pct? true}]
             [:split-hl-hue "HIGHLIGHT HUE" 0 360 1 {:decimals 0}]
             [:split-hl-sat "HIGHLIGHT SATURATION" 0 1 0.01 {:pct? true}]
             [:split-balance "BALANCE" -1 1 0.01 {:pct? true}]]
   :look  [[:bw "BLACK & WHITE" 0 1 0.01 {:pct? true}]
           [:fade "FADE" 0 1 0.01 {:pct? true}]
           [:grain "GRAIN" 0 1 0.01 {:pct? true}]
           [:vignette "VIGNETTE" 0 1 0.01 {:pct? true}]]
   :crop  [[:angle "STRAIGHTEN" -15 15 0.1 {:decimals 1}]]
   :lens  [[:persp-v "VERTICAL" -1 1 0.01 {:pct? true}]
           [:persp-h "HORIZONTAL" -1 1 0.01 {:pct? true}]
           [:distortion "DISTORTION" -1 1 0.01 {:pct? true}]
           [:ca-red "RED / CYAN FRINGE" -1 1 0.01 {:pct? true}]
           [:ca-blue "BLUE / YELLOW FRINGE" -1 1 0.01 {:pct? true}]]})

(def ^:private tab-labels
  [[:basic "BASIC"] [:detail "DETAIL"] [:color "COLOR"] [:curve "CURVE"] [:look "LOOK"] [:crop "CROP"]
   [:presets "PRESETS"] [:history "HISTORY"]])

(def ^:private whole-number-keys
  "Settings whose sliders step in whole numbers and are stored as integers."
  #{:denoise :denoise-color :sharpen :split-sh-hue :split-hl-hue})

(defn- sliders-for [tab]
  (vec (for [[k label mn mx step opts] (slider-specs tab)]
         (let [default (pipeline/default-settings k)
               row (w/slider-row (merge {:label label :min mn :max mx :step step :default default
                                         :value (double default)
                                         :on-input (fn [v] (st/set-adj! k (if (whole-number-keys k) (long v) v)))
                                         :on-commit (fn [] (st/commit! label))
                                         :on-reset (fn [] (st/reset-adj! k label default))}
                                        opts))]
           (assoc row :key k)))))

(defn- pill-row [items current on-pick]
  (let [fp (doto (FlowPane. 8.0 8.0))]
    (doseq [[v label] items]
      (let [b (w/pill label (fn [] (on-pick v)) "sm")]
        (w/set-on! b (= v current))
        (.add (.getChildren fp) b)))
    fp))

(defn- basic-like-body [tab]
  (let [rows (sliders-for tab)]
    {:node (apply w/vbox 18 (map :node rows))
     :sync! (fn [adj] (doseq [{:keys [key set-value!]} rows] (set-value! (get adj key))))}))

(defn- frame-aspect
  "Width / height of the current frame's uncropped, turned image."
  []
  (let [s @st/state adj (st/cur-adj s)]
    (canvas/frame-aspect (:cur s) (or (:rotate adj) 0))))

(def ^:private aspect-pills
  [["free" "FREE"] ["orig" "ORIGINAL"] ["1:1" "1:1"] ["4:5" "4:5"] ["3:2" "3:2"] ["16:9" "16:9"]])

(defn- pick-aspect!
  "Chooses the crop's aspect: FREE just releases the lock; ORIGINAL clears the
  crop; a preset becomes the largest centred crop of that ratio."
  [a]
  (st/set-adj! :aspect a)
  (case a
    "free" nil
    "orig" (st/set-adj! :crop nil)
    (st/set-adj! :crop (mapv double (crop/centered (geometry/aspect-ratios a) (frame-aspect)))))
  (st/commit! (str "CROP " (clojure.string/upper-case (if (= a "orig") "original" a)))))

(defn- turn!
  "Quarter-turn the picture by `d` (+1 clockwise); the old crop no longer fits."
  [d]
  (let [r (or (:rotate (st/cur-adj @st/state)) 0)]
    (st/set-adj! :rotate (mod (+ r d) 4))
    (st/set-adj! :crop nil)
    (st/commit! (if (pos? d) "ROTATE RIGHT" "ROTATE LEFT"))))

(defn- crop-rect-rows
  "Four sliders (LEFT, TOP, WIDTH, HEIGHT as % of the frame) for setting the
  crop precisely or from the keyboard."
  []
  (let [labels ["LEFT" "TOP" "WIDTH" "HEIGHT"]
        current (fn [] (let [adj (st/cur-adj @st/state) a (frame-aspect)]
                         (vec (geometry/crop-fractions adj a 1.0))))
        set-part! (fn [i v]
                    (let [[x y w h] (current)
                          r (assoc [x y w h] i (/ (double v) 100.0))
                          [x y w h] r
                          w (max crop/min-size (min 1.0 w)) h (max crop/min-size (min 1.0 h))]
                      (st/set-adj! :crop [(max 0.0 (min x (- 1.0 w))) (max 0.0 (min y (- 1.0 h))) w h])))]
    (vec (for [[i label] (map-indexed vector labels)]
           (assoc (w/slider-row {:label label :min 0 :max 100 :step 1 :decimals 0 :value (if (>= i 2) 100.0 0.0)
                                 :default (if (>= i 2) 100.0 0.0)
                                 :on-input  (fn [v] (set-part! i v))
                                 :on-commit (fn [] (st/commit! (str "CROP " label)))
                                 :on-reset  (fn [] (set-part! i (if (>= i 2) 100.0 0.0)) (st/commit! (str "CROP " label " RESET")))})
                  :i i)))))

(defn- crop-body []
  (let [angle  (first (sliders-for :crop))
        lens   (sliders-for :lens)
        rect   (crop-rect-rows)
        aspects (FlowPane. 8.0 8.0)
        pills  (into {} (for [[v label] aspect-pills]
                          [v (doto (w/pill label (fn [] (pick-aspect! v)) "sm")
                               (w/set-base-a11y! (str "Aspect " label)))]))
        flip-h (w/pill "FLIP HORIZONTAL" (fn [] (st/set-adj! :flip (not (:flip (st/cur-adj @st/state)))) (st/commit! "FLIP")) "sm")
        flip-v (w/pill "FLIP VERTICAL" (fn [] (st/set-adj! :flip-v (not (:flip-v (st/cur-adj @st/state)))) (st/commit! "FLIP VERTICAL")) "sm")
        left   (w/pill "↺ 90°" (fn [] (turn! -1)) "sm")
        right  (w/pill "↻ 90°" (fn [] (turn! 1)) "sm")
        reset  (w/button (theme/tracked "RESET CROP" :normal)
                         (fn [] (st/set-adj! :crop nil) (st/set-adj! :aspect "orig") (st/commit! "CROP RESET"))
                         "text-btn" "quiet")]
    (w/a11y! left "Rotate left 90 degrees")
    (w/a11y! right "Rotate right 90 degrees")
    (doseq [[_ b] pills] (.add (.getChildren aspects) b))
    {:node (w/vbox 18
                   (w/tlabel "ASPECT" :normal "sys" "dim") aspects
                   (doto (FlowPane. 8.0 8.0) (.. getChildren (addAll (java.util.Arrays/asList (into-array javafx.scene.Node [left right flip-h flip-v])))))
                   (:node angle)
                   (w/tlabel "CROP" :wide "sys" "dim") (apply w/vbox 18 (map :node rect))
                   reset
                   (w/tlabel "PERSPECTIVE & LENS" :wide "sys" "dim") (apply w/vbox 18 (map :node lens)))
     :sync! (fn [adj]
              ((:set-value! angle) (:angle adj))
              (w/set-on! flip-h (boolean (:flip adj)))
              (w/set-on! flip-v (boolean (:flip-v adj)))
              (doseq [[v b] pills] (w/set-on! b (= v (or (:aspect adj) "orig"))))
              (let [a (frame-aspect)
                    [x y w h] (geometry/crop-fractions adj a 1.0)]
                (doseq [{:keys [i set-value!]} rect] (set-value! (* 100.0 (nth [x y w h] i)))))
              (doseq [{:keys [key set-value!]} lens] (set-value! (get adj key))))}))

(defn- presets-body []
  (let [col (w/vbox 0)]
    (doseq [[n _] cat/presets]
      (let [b (doto (Button.) (.setMaxWidth Double/MAX_VALUE))]
        (w/classes! b "gh-btn" "preset-row")
        (.setGraphic b (let [row (HBox.)]
                         (.setAlignment row Pos/BASELINE_LEFT)
                         (w/add! row (doto (w/label n "display") (.setStyle "-fx-font-size: 26px;"))
                                 (w/spacer) (w/tlabel "APPLY" :wide "sys"))
                         row))
        (.setMaxWidth (.getGraphic b) Double/MAX_VALUE)
        (w/a11y! b (str "Apply preset " n))
        (.setOnAction b (w/handler (fn [_] (st/apply-preset! n))))
        (w/add! col b)))
    {:node col :sync! (fn [_] nil)}))

(defn- history-body []
  (let [col (w/vbox 0)]
    {:node col
     :sync! (fn [_]
              (let [h (:history (cat/frame (:catalog @st/state) (:cur @st/state)))
                    n (count h)]
                (w/keep-focus!
                  col
                  (fn []
                    (w/clear! col)
                    (doseq [[i {:keys [label]}] (reverse (map-indexed vector h))]
                      (let [b (w/button nil (fn [] (st/revert! i)) "text-btn" "short")
                            cur? (= i (dec n))]
                        (.setGraphic b (w/hbox 12 (doto (w/tlabel (format "%02d" (inc i)) :normal "sys-12" "sys" "faint") (.setMinWidth 24))
                                               (w/tlabel label :normal "sys-12" "sys" (if cur? "bone" "dim"))))
                        (.setText b "")
                        (w/a11y! b (str "Step " (inc i) ", " label (if cur? ", current" ", revert to this step")))
                        (w/add! col b)))))))}))

(defn- curve-body []
  (let [c (curve/create)
        channels [[:curve "RGB"] [:curve-r "RED"] [:curve-g "GREEN"] [:curve-b "BLUE"]]
        pills (into {} (for [[k label] channels]
                         [k (doto (w/pill label nil "sm") (w/set-base-a11y! (str label " curve")))]))
        fp    (doto (FlowPane. 8.0 8.0))
        show! (fn [] (doseq [[k b] pills] (w/set-on! b (= k ((:channel c))))))
        reset (w/button (theme/tracked "RESET CURVE" :normal)
                        (fn [] (let [k ((:channel c))]
                                 (st/set-adj! k develop/default-curve)
                                 (st/commit! (str (.toUpperCase ^String (curve/channel-labels k)) " CURVE RESET"))))
                        "text-btn" "quiet")]
    (doseq [[k b] pills]
      (.setOnAction b (w/handler (fn [_] ((:set-channel! c) k) (show!))))
      (.add (.getChildren fp) b))
    (show!)
    {:node (w/vbox 14 fp (:node c) reset)
     :sync! (fn [adj] ((:sync! c) adj) (show!))}))

(defn- color-body
  "HSL mixer (one band at a time) and split toning."
  []
  (let [band     (atom 0)
        names    (mapv first develop/hsl-bands)
        hsl-of   (fn [adj] (vec (get adj :hsl develop/default-hsl)))
        set-hsl! (fn [i v] (let [hsl (hsl-of (st/cur-adj @st/state))
                                 b   @band]
                             (st/set-adj! :hsl (assoc hsl b (assoc (nth hsl b) i v)))))
        rows     (vec (for [[i label] [[0 "HUE"] [1 "SATURATION"] [2 "LUMINANCE"]]]
                        (let [msg (fn [] (str (nth names @band) " " label))]
                          (w/slider-row {:label label :min -1 :max 1 :step 0.01 :default 0.0 :value 0.0 :pct? true
                                         :on-input  (fn [v] (set-hsl! i v))
                                         :on-commit (fn [] (st/commit! (msg)))
                                         :on-reset  (fn [] (set-hsl! i 0.0) (st/commit! (str (msg) " RESET")))}))))
        pills    (vec (for [[i n] (map-indexed vector names)]
                        (doto (w/pill n nil "sm") (w/set-base-a11y! (str n " band")))))
        fp       (doto (FlowPane. 8.0 8.0))
        grading  (sliders-for :grading)
        show!    (fn [adj]
                   (doseq [[i b] (map-indexed vector pills)] (w/set-on! b (= i @band)))
                   (let [[h s l] (nth (hsl-of adj) @band)]
                     (doseq [[r v] (map vector rows [h s l])] ((:set-value! r) v))))]
    (doseq [[i b] (map-indexed vector pills)]
      (.setOnAction b (w/handler (fn [_] (reset! band i) (show! (st/cur-adj @st/state))))))
    (doseq [b pills] (.add (.getChildren fp) b))
    {:node (w/vbox 18
                   (w/tlabel "HSL" :wide "sys" "dim") fp (apply w/vbox 18 (map :node rows))
                   (w/tlabel "SPLIT TONING" :wide "sys" "dim") (apply w/vbox 18 (map :node grading)))
     :sync! (fn [adj]
              (show! adj)
              (doseq [{:keys [key set-value!]} grading] (set-value! (get adj key))))}))

(defn- body-for [tab]
  (case tab
    (:basic :detail :look) (basic-like-body tab)
    :color   (color-body)
    :crop    (crop-body)
    :curve   (curve-body)
    :presets (presets-body)
    :history (history-body)))

;; ---------------------------------------------------------------- filmstrip

(defn- strip-thumb [path on?]
  (let [iv   (doto (ImageView.) (.setFitWidth 68.0) (.setFitHeight 64.0) (.setSmooth true))
        box  (doto (StackPane.) (.setMinSize 68 64) (.setPrefSize 68 64) (.setMaxSize 68 64))
        name (w/tlabel (cat/frame-name path) :tight "sys-10" "sys" (if on? "bone" "dim"))
        b    (Button.)]
    (w/classes! box "thumb-frame")
    (w/set-classes! box "on" on?)
    (.setClip box (doto (Rectangle. 68 64) (.setArcWidth 20) (.setArcHeight 20)))
    (.setMaxWidth name 68.0)
    (w/add! box iv)
    (thumbs/request! path (constantly true)
                     (fn [^Image img]
                       (let [w (.getWidth img) h (.getHeight img) tw 68.0 th 64.0
                             k (min (/ w tw) (/ h th)) vw (* tw k) vh (* th k)]
                         (.setViewport iv (Rectangle2D. (/ (- w vw) 2.0) (/ (- h vh) 2.0) vw vh))
                         (.setImage iv img))))
    (w/classes! b "gh-btn")
    (.setGraphic b (w/vbox 6 box name))
    (.setOpacity box (if on? 1.0 0.6))
    (w/a11y! b (str (cat/frame-name path) (if on? ", current frame" ", open this frame")))
    (.setOnAction b (w/handler (fn [_] (st/select! path))))
    b))

;; --------------------------------------------------------------------- view

(defn create
  "Builds the Develop view. Returns {:node :view (ImageView) :refresh!
  :update-histogram! :set-loading!}."
  []
  (let [iv       (doto (ImageView.) (.setPreserveRatio true) (.setSmooth true))
        hint-lbl (w/label "" "editorial")
        loading  (doto (w/label "developing…" "editorial") (.setVisible false) (.setMouseTransparent true)
                   (.setStyle "-fx-font-size: 22px;"))
        canvas   (doto (StackPane.) (.setPadding (Insets. 20)) (.setMinSize 0 0))
        name-lbl (w/label "" "display")
        rating   (w/label "" "sys-13" "sys" "muted")
        before   (w/pill "AFTER" (fn [] (swap! st/state update :before not)))
        hist     (histogram-view/create)
        overlay  (crop-overlay/create iv)
        tabs     (doto (FlowPane. 14.0 0.0) (.setPadding (Insets. 0 24 0 24)))
        tab-btns (vec (for [[k l] tab-labels]
                        [k (doto (w/button (theme/tracked l :normal) (fn [] (swap! st/state assoc :tab k)) "tab-btn")
                             (w/set-base-a11y! (str l " tab")))]))
        body-box (doto (VBox.) (.setPadding (Insets. 20 24 20 24)))
        body-scroll (doto (ScrollPane. body-box) (.setFitToWidth true) (.setHbarPolicy ScrollPane$ScrollBarPolicy/NEVER))
        panel    (doto (VBox.) (.setMinWidth 300) (.setPrefWidth 340))
        roll-row (doto (HBox. 14.0) (.setAlignment Pos/BOTTOM_LEFT))
        roll-scroll (doto (ScrollPane. roll-row) (.setFitToHeight true) (.setVbarPolicy ScrollPane$ScrollBarPolicy/NEVER)
                      (.setHbarPolicy ScrollPane$ScrollBarPolicy/NEVER) (.setMinHeight 100) (.setPrefHeight 100))
        roll-count (w/tlabel "" :normal "sys-11s" "faint")
        body     (atom nil)
        memo     (atom {})]
    (.bind (.fitWidthProperty iv) (.subtract (.widthProperty canvas) 40))
    (.bind (.fitHeightProperty iv) (.subtract (.heightProperty canvas) 40))
    (w/classes! canvas "ground")
    (w/add! canvas iv (:node overlay) loading)
    (w/classes! body-box "panel")
    (.setStyle body-scroll "-fx-background-color: transparent;")
    (apply w/add! tabs (map second tab-btns))
    (w/classes! tabs "rule-bottom")
    (w/a11y! (:node hist) "Luminance histogram of the edited image")
    (w/classes! panel "rule-left")
    (.setStyle hint-lbl "-fx-font-size: 16px;")
    (.setStyle name-lbl "-fx-font-size: 28px;")
    (.setText hint-lbl "\\ before / after · 1–5 rating · ← → frames")
    (let [footer (doto (FlowPane. 16.0 8.0) (.setAlignment Pos/CENTER_LEFT) (.setPadding (Insets. 10 24 10 24)))
          left   (doto (HBox. 16.0) (.setAlignment Pos/BASELINE_LEFT))
          right  (w/hbox 16 rating before)]
      (w/classes! footer "rule-top")
      (w/add! left name-lbl hint-lbl)
      (w/add! footer left right)
      (VBox/setVgrow canvas Priority/ALWAYS)
      (let [left-col (w/vbox 0 canvas footer)]
        (HBox/setHgrow left-col Priority/ALWAYS)
        (.setMinWidth left-col 0)
        (VBox/setVgrow body-scroll Priority/ALWAYS)
        (w/add! panel
                (doto (w/vbox 8 (w/tlabel "HISTOGRAM" :wide "sys" "faint") (:node hist))
                  (.setPadding (Insets. 18 24 10 24)))
                tabs body-scroll)
        (let [strip (doto (w/vbox 10
                                  (w/hbox 0 (w/tlabel "THE ROLL" :wide "sys" "sys-12") (w/spacer) roll-count)
                                  roll-scroll)
                      (.setPadding (Insets. 14 28 16 28)))
              main (doto (w/add! (HBox.) left-col panel) (.setFillHeight true))
              card (doto (VBox.) )]
          (w/classes! strip "rule-top")
          (VBox/setVgrow main Priority/ALWAYS)
          (w/add! card main strip)
          (w/classes! card "develop-card")
          (.setClip card (let [r (Rectangle.)]
                           (.bind (.widthProperty r) (.widthProperty card))
                           (.bind (.heightProperty r) (.heightProperty card))
                           (.setArcWidth r 56) (.setArcHeight r 56) r))
          (let [outer (doto (w/add! (StackPane.) card) (.setPadding (Insets. 20 32 20 32)))]
            (w/classes! outer "page-cream")
            {:node outer
             :panel panel
             :view iv
             :update-histogram! (:update! hist)
             :set-loading! (fn [on?] (.setVisible loading (boolean on?)))
             :refresh!
             (fn refresh! [s]
               (let [c (:catalog s) path (:cur s)
                     fs (st/frames s)
                     adj (when path (cat/adj c path))
                     tab (:tab s)]
                 (.setText name-lbl (if path (cat/frame-name path) ""))
                 (.setText rating (theme/tracked (dots (if path (cat/rating c path) 0)) :wide))
                 (w/a11y! rating (str "Rating " (if path (cat/rating c path) 0) " of 5"))
                 (.setText before (theme/tracked (if (:before s) "BEFORE" "AFTER") :normal))
                 (w/set-classes! before "on" (boolean (:before s)))
                 (w/a11y! before (if (:before s)
                                   "Showing the original. Activate to show the edited image."
                                   "Showing the edited image. Activate to compare with the original."))
                 (w/a11y! iv (if path (str "Photo " (cat/frame-name path) (when (:before s) ", original") ", preview") "No photo"))
                 ;; tabs are built once (so keyboard focus stays on them) and only restyled
                 (doseq [[k b] tab-btns]
                   (w/set-on! b (= k tab)))
                 ;; panel body: rebuild on tab / frame change, otherwise just sync values
                 (when (or (not= (:tab @memo) tab) (not= (:cur @memo) path))
                   (reset! body (body-for tab))
                   (.clear (.getChildren body-box))
                   (.add (.getChildren body-box) (:node @body)))
                 (when (and @body adj) ((:sync! @body) adj))
                 ((:refresh! overlay) s)
                 ;; filmstrip
                 (let [sig [fs path (mapv #(cat/edited? c %) fs)]]
                   (when (not= sig (:strip @memo))
                     (w/keep-focus! roll-row
                                    (fn []
                                      (.clear (.getChildren roll-row))
                                      (doseq [p fs] (w/add! roll-row (strip-thumb p (= p path)))))
                                    (fn [r] (let [i (.indexOf ^java.util.List fs (:cur @st/state))]
                                              (when (>= i 0) (.get (.getChildren r) i)))))
                     (.setText roll-count (theme/tracked (str (count fs) " FRAMES · "
                                                              (count (filter #(cat/edited? c %) fs)) " EDITED") :normal))))
                 (swap! memo assoc :tab tab :cur path :strip [fs path (mapv #(cat/edited? c %) fs)])))}))))))
