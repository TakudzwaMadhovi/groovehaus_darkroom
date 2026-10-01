(ns darkroom.ui.develop
  "Develop view: canvas with before/after, histogram, tabbed adjustment panel
  (BASIC … HISTORY, INFO) and the filmstrip."
  (:require [darkroom.catalog :as cat]
            [darkroom.imaging.auto :as auto]
            [darkroom.imaging.crop :as crop]
            [darkroom.imaging.histogram :as histogram]
            [darkroom.imaging.local :as local]
            [darkroom.imaging.paths :as paths]
            [darkroom.imaging.raw :as raw]
            [darkroom.imaging.develop :as develop]
            [darkroom.imaging.geometry :as geometry]
            [darkroom.imaging.pipeline :as pipeline]
            [darkroom.ui.canvas :as canvas]
            [darkroom.ui.crop-overlay :as crop-overlay]
            [darkroom.ui.curve :as curve]
            [darkroom.ui.local-overlay :as local-overlay]
            [darkroom.ui.histogram-view :as histogram-view]
            [darkroom.ui.info :as info]
            [darkroom.ui.state :as st]
            [darkroom.ui.theme :as theme]
            [darkroom.ui.thumbs :as thumbs]
            [darkroom.ui.widgets :as w])
  (:import (javafx.geometry Insets Pos Rectangle2D)
           (javafx.scene.control Button Label ScrollPane ScrollPane$ScrollBarPolicy TextField)
           (javafx.stage FileChooser)
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
   [:local "LOCAL"] [:spots "SPOTS"] [:presets "PRESETS"] [:history "HISTORY"] [:info "INFO"]])

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

(defn- with-preview!
  "Calls (f preview-scene adjustments) for the current frame, or says it is
  still loading."
  [f]
  (let [s @st/state sess (canvas/session (:cur s))]
    (if sess (f (:preview sess) (st/cur-adj s)) (st/toast! "STILL LOADING"))))

(defn- auto-tone! []
  (with-preview!
    (fn [preview adj]
      (st/set-adjs! (auto/auto-tone preview (select-keys adj [:temp :tint])))
      (st/commit! "AUTO TONE"))))

(defn- auto-wb! []
  (with-preview!
    (fn [preview _]
      (st/set-adjs! (auto/auto-wb preview))
      (st/commit! "AUTO WHITE BALANCE"))))

(defn- toggle-pick! []
  (let [on? (not= :wb (:pick @st/state))]
    (swap! st/state assoc :pick (when on? :wb))
    (when on? (st/toast! "CLICK A NEUTRAL GREY OR WHITE"))))

(defn- pick-wb!
  "Sets temperature and tint from the colour under a click on the canvas image
  `iv`, read from the cropped/turned source before any tone edit."
  [^javafx.scene.input.MouseEvent e ^ImageView iv]
  (let [s @st/state sess (canvas/session (:cur s))]
    (when-let [img (and sess (pipeline/cached-image (:renderer sess) :geometry))]
      (let [b  (.getBoundsInLocal iv)
            w  (long (:width img)) h (long (:height img))
            px (min (dec w) (max 0 (long (* w (/ (.getX e) (max 1.0 (.getWidth b)))))))
            py (min (dec h) (max 0 (long (* h (/ (.getY e) (max 1.0 (.getHeight b)))))))]
        (st/set-adjs! (auto/wb-from-color (auto/average-color img px py 5)))
        (st/commit! "WHITE BALANCE PICK")
        (swap! st/state assoc :pick nil)))))

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

(defn- basic-body
  "BASIC: the AUTO / AUTO WB / PICK WB row above the sliders."
  []
  (let [base (basic-like-body :basic)
        auto-btn (doto (w/pill "AUTO" auto-tone! "sm") (w/set-base-a11y! "Auto tone"))
        wb-btn   (doto (w/pill "AUTO WB" auto-wb! "sm") (w/set-base-a11y! "Auto white balance"))
        pick-btn (doto (w/pill "PICK WB" toggle-pick! "sm") (w/set-base-a11y! "Pick white balance from the photo"))
        row      (doto (FlowPane. 8.0 8.0) (.. getChildren (addAll (java.util.Arrays/asList (into-array javafx.scene.Node [auto-btn wb-btn pick-btn])))))]
    {:node (w/vbox 18 row (:node base))
     :sync! (fn [adj]
              ((:sync! base) adj)
              (w/set-on! pick-btn (= :wb (:pick @st/state))))}))


;; -------------------------------------------------------------------- local

(def ^:private local-adjust-specs
  "[key label min max step opts] for a layer's adjustment sliders."
  [[:exposure "EXPOSURE" -2 2 0.01 {}]
   [:contrast "CONTRAST" -1 1 0.01 {:pct? true}]
   [:highlights "HIGHLIGHTS" -1 1 0.01 {:pct? true}]
   [:shadows "SHADOWS" -1 1 0.01 {:pct? true}]
   [:whites "WHITES" -1 1 0.01 {:pct? true}]
   [:blacks "BLACKS" -1 1 0.01 {:pct? true}]
   [:temp "TEMPERATURE" -1 1 0.01 {:pct? true}]
   [:tint "TINT" -1 1 0.01 {:pct? true}]
   [:saturation "SATURATION" -1 1 0.01 {:pct? true}]
   [:vibrance "VIBRANCE" -1 1 0.01 {:pct? true}]
   [:texture "TEXTURE" -1 1 0.01 {:pct? true}]
   [:clarity "CLARITY" -1 1 0.01 {:pct? true}]
   [:sharpen "SHARPEN" 0 100 1 {:decimals 0}]])

(defn- layer-slider
  "A slider bound to `(get-in layer path)` of layer `id`. Returns the slider-row map."
  [id label path lo hi step opts default]
  (w/slider-row (merge {:label label :min lo :max hi :step step :default default :value (double default)
                        :on-input  (fn [v] (st/update-layer! id #(assoc-in % path (if (:decimals opts) (long v) v))))
                        :on-commit (fn [] (st/commit! (str "LOCAL " label)))
                        :on-reset  (fn [] (st/update-layer! id #(assoc-in % path default)) (st/commit! (str "LOCAL " label " RESET")))}
                       opts)))

(defn- toggle-pill [label on-pick a11y]
  (doto (w/pill label on-pick "sm") (w/set-base-a11y! a11y)))

(defn- pill-flow
  "Pills laid out in a wrapping row (so they never get clipped in the narrow panel)."
  [& nodes]
  (let [fp (FlowPane. 8.0 8.0)]
    (doseq [n nodes] (.add (.getChildren fp) n))
    fp))

(defn- layer-editor
  "Controls for layer `layer`: returns {:node :sync! (fn [layer])}."
  [layer]
  (let [id (:id layer)
        adj-rows (vec (for [[k label lo hi step opts] local-adjust-specs]
                        (assoc (layer-slider id label [:adj k] lo hi step opts 0.0) :path [:adj k])))
        amount   (assoc (layer-slider id "AMOUNT" [:amount] 0 1 0.01 {:pct? true} 1.0) :path [:amount])
        feather  (assoc (layer-slider id "FEATHER" [:feather] 0 1 0.01 {:pct? true} 0.0) :path [:feather])
        invert   (toggle-pill "INVERT" (fn [] (st/update-layer! id #(update % :invert not)) (st/commit! "INVERT MASK")) "Invert the mask")
        visible  (toggle-pill "HIDE" (fn [] (st/update-layer! id #(update % :visible (fn [v] (false? v)))) (st/commit! "TOGGLE LAYER")) "Hide this layer")
        delete   (toggle-pill "DELETE" (fn [] (st/delete-layer! id)) "Delete this layer")
        show     (toggle-pill "SHOW MASK" (fn [] (swap! st/state update :local-mask not)) "Show the mask on the photo")
        ;; range limits
        luma-on  (toggle-pill "LIMIT BY LIGHT" nil "Limit this layer to a range of brightness")
        luma-lo  (assoc (layer-slider id "FROM" [:range :luma :lo] 0 1 0.01 {:pct? true} 0.0) :path [:range :luma :lo])
        luma-hi  (assoc (layer-slider id "TO" [:range :luma :hi] 0 1 0.01 {:pct? true} 1.0) :path [:range :luma :hi])
        luma-sm  (assoc (layer-slider id "SOFTNESS" [:range :luma :smooth] 0 0.5 0.01 {:pct? true} 0.1) :path [:range :luma :smooth])
        col-on   (toggle-pill "LIMIT BY COLOUR" nil "Limit this layer to a range of colours")
        col-hue  (assoc (layer-slider id "HUE" [:range :color :hue] 0 360 1 {:decimals 0} 0) :path [:range :color :hue])
        col-rng  (assoc (layer-slider id "RANGE" [:range :color :range] 1 90 1 {:decimals 0} 20) :path [:range :color :range])
        ;; brush
        tool-row (fn [label k lo hi step opts default]
                   (w/slider-row (merge {:label label :min lo :max hi :step step :default default :value (double default)
                                         :on-input (fn [v] (st/set-tool! k v))}
                                        opts)))
        b-size   (tool-row "BRUSH SIZE" :brush-size 0.005 0.15 0.005 {:pct? true} 0.04)
        b-feath  (tool-row "BRUSH FEATHER" :brush-feather 0 1 0.01 {:pct? true} 0.5)
        b-flow   (tool-row "BRUSH FLOW" :brush-flow 0.05 1 0.01 {:pct? true} 1.0)
        erase    (toggle-pill "ERASE" (fn [] (swap! st/state update-in [:tool :erase] not)) "Erase brush strokes")
        clear    (toggle-pill "CLEAR STROKES" (fn [] (st/update-layer! id #(assoc-in % [:shape :strokes] [])) (st/commit! "CLEAR STROKES")) "Remove all brush strokes")
        hint     (case (:type layer)
                   :linear  "DRAG THE END POINTS ON THE PHOTO"
                   :radial  "DRAG THE CENTRE OR SIDE HANDLES"
                   :brush   "PAINT ON THE PHOTO"
                   :subject "DRAG A BOX AROUND THE SUBJECT"
                   :subject-ai "SELECTED BY A NEURAL NETWORK; REFINE WITH AMOUNT, FEATHER AND INVERT"
                   :sky     "SELECTED FROM COLOUR AND BRIGHTNESS (APPROXIMATE)"
                   :range   "AFFECTS THE WHOLE PHOTO, LIMITED BY THE RANGES BELOW"
                   "")
        limits? (atom (boolean (or (:luma (:range layer)) (:color (:range layer)))))]
    (let [range-rows [luma-lo luma-hi luma-sm col-hue col-rng]
          all-rows   (concat adj-rows [amount feather] range-rows)
          shell      (w/vbox 18
                             (w/tlabel hint :normal "sys-10" "faint")
                             (pill-flow show invert visible delete)
                             (when (= :brush (:type layer)) (w/vbox 14 (:node b-size) (:node b-feath) (:node b-flow) (pill-flow erase clear)))
                             (w/tlabel "ADJUST" :wide "sys" "dim")
                             (apply w/vbox 18 (map :node adj-rows))
                             (:node amount) (:node feather)
                             (w/tlabel "LIMITS" :wide "sys" "dim")
                             (pill-flow luma-on col-on)
                             (w/vbox 14 (:node luma-lo) (:node luma-hi) (:node luma-sm))
                             (w/vbox 14 (:node col-hue) (:node col-rng)))
          set-range! (fn [k v on?]
                       (st/update-layer! id (fn [l] (if on? (assoc-in l [:range k] v) (update l :range dissoc k))))
                       (st/commit! (str "LIMIT " (name k))))]
      (.setOnAction luma-on (w/handler (fn [_] (set-range! :luma {:lo 0.5 :hi 1.0 :smooth 0.1} (not (get-in (st/selected-layer) [:range :luma]))))))
      (.setOnAction col-on  (w/handler (fn [_] (set-range! :color {:hue 0 :range 20} (not (get-in (st/selected-layer) [:range :color]))))))
      {:node shell
       :sync! (fn [l]
                (doseq [{:keys [path set-value!]} all-rows]
                  (when-let [v (get-in l path)] (set-value! v)))
                (w/set-on! invert (boolean (:invert l)))
                (w/set-on! visible (false? (:visible l)))
                (w/set-on! show (boolean (:local-mask @st/state)))
                (w/set-on! erase (boolean (get-in @st/state [:tool :erase])))
                (w/set-on! luma-on (boolean (get-in l [:range :luma])))
                (w/set-on! col-on (boolean (get-in l [:range :color])))
                (let [t (:tool @st/state)]
                  ((:set-value! b-size) (:brush-size t)) ((:set-value! b-feath) (:brush-feather t)) ((:set-value! b-flow) (:brush-flow t))))})))

(defn- local-body
  "LOCAL tab: add a layer, pick one from the list, edit it."
  []
  (let [root   (w/vbox 16)
        adders (FlowPane. 8.0 8.0)
        list-box (w/vbox 0)
        editor (w/vbox 0)
        sig    (atom nil)
        cur-editor (atom nil)]
    (doseq [t [:linear :radial :brush :range :subject :subject-ai :sky]]
      (.add (.getChildren adders)
            (doto (w/pill (if (= t :subject-ai) "+ AI SUBJECT" (str "+ " (clojure.string/upper-case (name t)))) (fn [] (st/add-layer! t)) "sm")
              (w/set-base-a11y! (str "Add " (local/type-labels t))))))
    (.add (.getChildren adders)
          (doto (w/pill "AI MODEL…" (fn []
                                      (let [ch (doto (javafx.stage.FileChooser.) (.setTitle "Subject segmentation model (U²-Net ONNX)"))]
                                        (.add (.getExtensionFilters ch) (javafx.stage.FileChooser$ExtensionFilter. "ONNX model" ["*.onnx"]))
                                        (when-let [f (.showOpenDialog ch (first (javafx.stage.Window/getWindows)))]
                                          (when (st/set-segment-model! f) (st/toast! "AI MODEL READY")))))
                        "sm")
            (w/set-base-a11y! "Choose the AI subject model file")))
    (w/add! root (w/tlabel "ADD A MASK" :wide "sys" "dim") adders (w/tlabel "MASKS" :wide "sys" "dim") list-box editor)
    {:node root
     :sync! (fn [adj]
              (let [layers (vec (:local adj)) sel (:local-sel @st/state)
                    new-sig [(mapv #(select-keys % [:id :type :visible]) layers) sel (mapv local/layer-title layers)]]
                (when (not= new-sig @sig)
                  (reset! sig new-sig)
                  (w/keep-focus!
                    root
                    (fn []
                      (w/clear! list-box) (w/clear! editor)
                      (when (empty? layers)
                        (w/add! list-box (w/label "no masks yet: add one above, then adjust it." "editorial")))
                      (doseq [l layers]
                        (let [b (w/button nil (fn [] (st/select-layer! (:id l))) "text-btn" "short")
                              on? (= sel (:id l))]
                          (.setGraphic b (w/hbox 12 (w/tlabel (format "%02d" (:id l)) :normal "sys-12" "sys" "faint")
                                                 (w/tlabel (local/layer-title l) :normal "sys-12" "sys" (if on? "bone" "dim"))))
                          (.setText b "")
                          (w/a11y! b (str "Layer " (:id l) ", " (local/layer-title l) (when on? ", selected") (when (false? (:visible l)) ", hidden")))
                          (w/add! list-box b)))
                      (reset! cur-editor
                              (when-let [l (first (filter #(= sel (:id %)) layers))]
                                (let [e (layer-editor l)] (w/add! editor (:node e)) e))))))
                (when-let [e @cur-editor]
                  (when-let [l (first (filter #(= (:local-sel @st/state) (:id %)) (:local adj)))]
                    ((:sync! e) l)))))}))

(defn- spots-body
  "SPOTS tab: heal or clone small blemishes by clicking the photo."
  []
  (let [root (w/vbox 18)
        modes (FlowPane. 8.0 8.0)
        heal  (toggle-pill "HEAL" (fn [] (st/set-tool! :spot-mode :heal)) "Heal spots")
        clone (toggle-pill "CLONE" (fn [] (st/set-tool! :spot-mode :clone)) "Clone spots")
        size  (w/slider-row {:label "SIZE" :min 0.004 :max 0.08 :step 0.002 :default 0.02 :value 0.02 :pct? true
                             :on-input (fn [v] (st/set-tool! :spot-size v))})
        list-box (w/vbox 0)
        clear (w/button (theme/tracked "CLEAR ALL SPOTS" :normal) (fn [] (st/clear-spots!)) "text-btn" "quiet")
        sig (atom nil)]
    (.add (.getChildren modes) heal) (.add (.getChildren modes) clone)
    (w/add! root (w/tlabel "CLICK THE PHOTO TO REMOVE A BLEMISH" :normal "sys-10" "faint")
            (w/tlabel "MODE" :wide "sys" "dim") modes (:node size)
            (w/tlabel "SPOTS" :wide "sys" "dim") list-box clear)
    {:node root
     :sync! (fn [adj]
              (let [t (:tool @st/state)]
                (w/set-on! heal (= :heal (:spot-mode t))) (w/set-on! clone (= :clone (:spot-mode t)))
                ((:set-value! size) (:spot-size t)))
              (let [sp (vec (:spots adj))]
                (when (not= sp @sig)
                  (reset! sig sp)
                  (w/keep-focus!
                    list-box
                    (fn []
                      (w/clear! list-box)
                      (when (empty? sp) (w/add! list-box (w/label "no spots yet." "editorial")))
                      (doseq [[i s] (map-indexed vector sp)]
                        (let [b (w/button nil (fn [] (st/delete-spot! i)) "text-btn" "short")]
                          (.setGraphic b (w/hbox 12 (w/tlabel (format "%02d" (inc i)) :normal "sys-12" "sys" "faint")
                                                 (w/tlabel (str (clojure.string/upper-case (name (:mode s))) " · DELETE") :normal "sys-12" "sys" "dim")))
                          (.setText b "")
                          (w/a11y! b (str "Spot " (inc i) ", " (name (:mode s)) ", activate to delete"))
                          (w/add! list-box b))))))))}))

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
        lens-name (w/tlabel "" :normal "sys-10" "faint")
        lens-auto (doto (w/pill "AUTO LENS PROFILE" (fn [] (st/apply-lens-profile!)) "sm")
                    (w/set-base-a11y! "Apply the lens profile for this photo's lens"))
        lens-off  (doto (w/pill "PROFILE OFF" (fn [] (st/clear-lens-profile!)) "sm")
                    (w/set-base-a11y! "Remove the lens profile"))
        lens-db   (doto (w/pill "LENS DATABASE…" (fn []
                                                    (let [ch (doto (javafx.stage.DirectoryChooser.)
                                                               (.setTitle "Folder of lensfun XML files (the db folder)"))]
                                                      (when-let [d (.showDialog ch (first (javafx.stage.Window/getWindows)))]
                                                        (st/load-lens-db! d))))
                                  "sm")
                    (w/set-base-a11y! "Choose the folder of lensfun lens profile files"))
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
                   (w/tlabel "LENS PROFILE" :wide "sys" "dim")
                   (doto (FlowPane. 8.0 8.0) (.. getChildren (addAll (java.util.Arrays/asList (into-array javafx.scene.Node [lens-auto lens-off lens-db])))))
                   lens-name
                   (w/tlabel "PERSPECTIVE & LENS" :wide "sys" "dim") (apply w/vbox 18 (map :node lens)))
     :sync! (fn [adj]
              ((:set-value! angle) (:angle adj))
              (w/set-on! flip-h (boolean (:flip adj)))
              (w/set-on! flip-v (boolean (:flip-v adj)))
              (doseq [[v b] pills] (w/set-on! b (= v (or (:aspect adj) "orig"))))
              (let [a (frame-aspect)
                    [x y w h] (geometry/crop-fractions adj a 1.0)]
                (doseq [{:keys [i set-value!]} rect] (set-value! (* 100.0 (nth [x y w h] i)))))
              (doseq [{:keys [key set-value!]} lens] (set-value! (get adj key)))
              (let [p (:lens-profile adj)]
                (w/set-on! lens-off (boolean p))
                (.setText lens-name (theme/tracked (cond p (str "ACTIVE: " (.toUpperCase ^String (:name p)))
                                                         (empty? @st/lens-db) "NO LENS DATABASE LOADED"
                                                         :else (str (count @st/lens-db) " LENSES IN DATABASE")) :normal))))}))

(defn- row-button
  "A full-width list row: `title` on the left, `action-text` on the right."
  [title action-text a11y on-action & classes]
  (let [b (doto (Button.) (.setMaxWidth Double/MAX_VALUE))
        row (HBox.)]
    (apply w/classes! b "gh-btn" "preset-row" classes)
    (.setAlignment row Pos/BASELINE_LEFT)
    (w/add! row (doto (w/label title "display") (.setStyle "-fx-font-size: 26px;")) (w/spacer) (w/tlabel action-text :wide "sys"))
    (.setMaxWidth row Double/MAX_VALUE)
    (.setGraphic b row)
    (w/a11y! b a11y)
    (.setOnAction b (w/handler (fn [_] (on-action))))
    b))

(defn- file-chooser [title] (doto (FileChooser.) (.setTitle title)
                              (-> .getExtensionFilters (.add (javafx.stage.FileChooser$ExtensionFilter. "Presets" ["*.edn"])))))

(defn- presets-body []
  (let [mine      (w/vbox 0)
        name-in   (doto (TextField.) (.setPromptText "name the current look"))
        save!     (fn []
                    (let [n (.getText name-in)]
                      (if (st/save-preset! n)
                        (do (.clear name-in) (st/toast! (str "SAVED " (clojure.string/upper-case (clojure.string/trim n)))))
                        (st/toast! "NAME THE PRESET FIRST"))))
        owner     (fn [] (some-> name-in .getScene .getWindow))
        copy-row  (doto (FlowPane. 8.0 8.0)
                    (w/add! (doto (w/pill "COPY SETTINGS" (fn [] (when (st/copy-settings!) (st/toast! "SETTINGS COPIED"))) "xs")
                              (w/set-base-a11y! "Copy this frame's settings"))
                            (doto (w/pill "PASTE" (fn [] (if-let [n (st/paste-settings!)] (st/toast! (str n " PASTED")) (st/toast! "COPY SETTINGS FIRST"))) "xs")
                              (w/set-base-a11y! "Paste copied settings"))))
        io-row    (doto (FlowPane. 8.0 8.0)
                    (w/add! (doto (w/pill "IMPORT" (fn []
                                                      (when-let [f (.showOpenDialog (file-chooser "Import presets") (owner))]
                                                        (let [n (try (st/import-presets! f) (catch Exception _ 0))]
                                                          (st/toast! (if (pos? n) (str n " PRESETS IMPORTED") "NO PRESETS FOUND")))))
                                          "xs")
                              (w/set-base-a11y! "Import presets from a file"))
                            (doto (w/pill "EXPORT" (fn []
                                                      (when-let [f (.showSaveDialog (file-chooser "Export presets") (owner))]
                                                        (st/toast! (str (st/export-presets! f) " PRESETS EXPORTED"))))
                                          "xs")
                              (w/set-base-a11y! "Export your presets to a file"))))
        built-in  (apply w/vbox 0 (for [[n _] cat/presets]
                                    (row-button n "APPLY" (str "Apply preset " n) (fn [] (st/apply-preset! n)))))
        memo      (atom nil)]
    (w/classes! name-in "gh-input")
    (w/a11y! name-in "Preset name")
    (.setOnKeyPressed name-in (w/handler (fn [^javafx.scene.input.KeyEvent e]
                                           (when (= (.getCode e) javafx.scene.input.KeyCode/ENTER) (save!) (.consume e)))))
    {:node (w/vbox 14
                   copy-row
                   (w/tlabel "MY PRESETS" :wide "sys" "dim")
                   (w/hbox 8 name-in (w/button (theme/tracked "SAVE" :normal) save! "text-btn" "sun" "short"))
                   mine io-row
                   (w/tlabel "BUILT-IN" :wide "sys" "dim") built-in)
     :sync! (fn [_]
              (let [names (mapv first (cat/user-presets (:catalog @st/state)))]
                (when (not= names @memo)
                  (reset! memo names)
                  (w/keep-focus!
                    mine
                    (fn []
                      (w/clear! mine)
                      (doseq [n names]
                        (let [del (doto (w/pill "×" (fn [] (st/delete-preset! n)) "xs") (w/set-base-a11y! (str "Delete preset " n)))
                              row (row-button n "APPLY" (str "Apply preset " n) (fn [] (st/apply-user-preset! n)))]
                          (w/add! mine (w/hbox 8 (doto row (HBox/setHgrow Priority/ALWAYS)) del))))
                      (when (empty? names)
                        (w/add! mine (doto (w/label "none yet — save the current look above." "editorial") (.setStyle "-fx-font-size: 16px;")))))))))}))

(defn- history-body []
  (let [col     (w/vbox 0)
        snaps   (w/vbox 0)
        name-in (doto (TextField.) (.setPromptText "name this state"))
        snap!   (fn [] (if (st/add-snapshot! (.getText name-in)) (.clear name-in) (st/toast! "NAME THE SNAPSHOT FIRST")))
        memo    (atom nil)
        undo    (doto (w/pill "UNDO" (fn [] (st/undo!)) "xs") (w/set-base-a11y! "Undo (Ctrl or Command Z)"))
        redo    (doto (w/pill "REDO" (fn [] (st/redo!)) "xs") (w/set-base-a11y! "Redo (Ctrl or Command Shift Z)"))]
    (w/classes! name-in "gh-input")
    (w/a11y! name-in "Snapshot name")
    (.setOnKeyPressed name-in (w/handler (fn [^javafx.scene.input.KeyEvent e]
                                           (when (= (.getCode e) javafx.scene.input.KeyCode/ENTER) (snap!) (.consume e)))))
    {:node (w/vbox 14
                   (w/hbox 8 undo redo)
                   (w/tlabel "SNAPSHOTS" :wide "sys" "dim")
                   (w/hbox 8 name-in (w/button (theme/tracked "SAVE" :normal) snap! "text-btn" "sun" "short"))
                   snaps
                   (w/tlabel "HISTORY" :wide "sys" "dim")
                   col)
     :sync! (fn [_]
              (let [s @st/state p (:cur s) c (:catalog s)
                    h (:history (cat/frame c p)) pos (cat/history-pos c p)
                    sn (cat/snapshots c p)]
                (.setDisable undo (not (cat/can-undo? c p)))
                (.setDisable redo (not (cat/can-redo? c p)))
                (when (not= [(mapv :label h) pos (mapv :name sn)] @memo)
                  (reset! memo [(mapv :label h) pos (mapv :name sn)])
                (w/keep-focus!
                  snaps
                  (fn []
                    (w/clear! snaps)
                    (doseq [[i {:keys [name]}] (map-indexed vector sn)]
                      (let [del (doto (w/pill "×" (fn [] (st/delete-snapshot! i)) "xs") (w/set-base-a11y! (str "Delete snapshot " name)))
                            b   (w/button nil (fn [] (st/apply-snapshot! i)) "text-btn" "short")]
                        (.setGraphic b (w/tlabel name :normal "sys-12" "sys" "bone"))
                        (.setText b "")
                        (w/a11y! b (str "Restore snapshot " name))
                        (w/add! snaps (w/hbox 8 (doto b (HBox/setHgrow Priority/ALWAYS)) del))))))
                (w/keep-focus!
                  col
                  (fn []
                    (w/clear! col)
                    (doseq [[i {:keys [label]}] (reverse (map-indexed vector h))]
                      (let [b    (w/button nil (fn [] (st/revert! i)) "text-btn" "short")
                            cur? (= i pos)
                            redo? (> i pos)]
                        (.setGraphic b (w/hbox 12 (doto (w/tlabel (format "%02d" (inc i)) :normal "sys-12" "sys" "faint") (.setMinWidth 24))
                                               (w/tlabel label :normal "sys-12" "sys" (if cur? "bone" "dim"))))
                        (.setText b "")
                        (when redo? (.setOpacity b 0.5))
                        (w/a11y! b (str "Step " (inc i) ", " label (cond cur? ", current" redo? ", undone, restore" :else ", revert to this step")))
                        (w/add! col b))))))))}))

(defn- info-body []
  (let [panel (info/create)]
    {:node (:node panel) :sync! (fn [_] ((:sync! panel) @st/state))}))

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

(defn- profile-section
  "Camera profile picker for RAW files: DEFAULT (LibRaw's built-in matrix) or
  one of the .dcp profiles the catalog knows. Returns {:node :sync! (fn [adj])}."
  []
  (let [pills   (FlowPane. 8.0 8.0)
        tools   (FlowPane. 8.0 8.0)
        note    (w/tlabel "" :normal "sys-10" "faint")
        memo    (atom nil)
        add-btn (doto (w/pill "ADD PROFILE…"
                              (fn []
                                (let [ch (doto (javafx.stage.FileChooser.) (.setTitle "Camera profiles (.dcp)"))]
                                  (.add (.getExtensionFilters ch) (javafx.stage.FileChooser$ExtensionFilter. "DNG camera profile" ["*.dcp" "*.DCP"]))
                                  (when-let [fs (.showOpenMultipleDialog ch (first (javafx.stage.Window/getWindows)))]
                                    (let [{:keys [added failed]} (st/add-camera-profiles! fs)]
                                      (st/toast! (cond (and (pos? added) (empty? failed)) (str added " PROFILE" (when (not= added 1) "S") " ADDED")
                                                       (pos? added) (str added " ADDED · " (count failed) " UNREADABLE")
                                                       :else "NOT A CAMERA PROFILE"))))))
                              "sm")
                  (w/set-base-a11y! "Add camera profile files"))
        auto-btn  (doto (w/pill "MATCH MY CAMERA" (fn [] (st/auto-camera-profile!)) "sm")
                    (w/set-base-a11y! "Pick the profile made for this photo's camera"))
        curve-btn (doto (w/pill "PROFILE TONE CURVE" (fn [] (st/toggle-profile-curve!)) "sm")
                    (w/set-base-a11y! "Use the profile's own tone curve"))]
    (.addAll (.getChildren tools) (java.util.Arrays/asList (into-array javafx.scene.Node [add-btn auto-btn curve-btn])))
    {:node (w/vbox 10 (w/tlabel "CAMERA PROFILE" :wide "sys" "dim") pills tools note)
     :sync! (fn [adj]
              (let [path (:cur @st/state)
                    raw? (boolean (and path (raw/raw-file? (paths/source-file path))))
                    profiles (st/camera-profiles)
                    cur (:camera-profile adj)
                    sig [profiles cur raw?]]
                (when (not= sig @memo)
                  (reset! memo sig)
                  (w/keep-focus!
                    pills
                    (fn []
                      (w/clear! pills)
                      (let [def-b (doto (w/pill "DEFAULT" (fn [] (st/set-camera-profile! nil)) "sm")
                                    (w/set-base-a11y! "Default camera colour (built-in matrix)"))]
                        (w/set-on! def-b (nil? cur)) (.setDisable def-b (not raw?))
                        (w/add! pills def-b))
                      (doseq [{:keys [path name]} profiles]
                        (let [b (doto (w/pill (.toUpperCase ^String name) (fn [] (st/set-camera-profile! path)) "sm")
                                  (w/set-base-a11y! (str "Camera profile " name)))
                              x (doto (w/pill "×" (fn [] (st/remove-camera-profile! path)) "sm")
                                  (w/set-base-a11y! (str "Remove profile " name)))]
                          (w/set-on! b (= path cur)) (.setDisable b (not raw?))
                          (w/add! pills b x)))))
                  (.setText note (theme/tracked (cond (not raw?) "CAMERA PROFILES APPLY TO RAW FILES ONLY"
                                                      (empty? profiles) "ADD A .DCP FILE (FROM LIGHTROOM / CAMERA RAW / DNG PROFILE EDITOR)"
                                                      :else "") :normal)))
                (w/set-on! curve-btn (boolean (:camera-profile-curve adj)))
                (.setDisable curve-btn (or (not raw?) (nil? cur)))
                (.setDisable auto-btn (not raw?))))}))

(defn- color-body
  "Camera profile, HSL mixer (one band at a time) and split toning."
  []
  (let [profile  (profile-section)
        band     (atom 0)
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
                   (:node profile)
                   (w/tlabel "HSL" :wide "sys" "dim") fp (apply w/vbox 18 (map :node rows))
                   (w/tlabel "SPLIT TONING" :wide "sys" "dim") (apply w/vbox 18 (map :node grading)))
     :sync! (fn [adj]
              ((:sync! profile) adj)
              (show! adj)
              (doseq [{:keys [key set-value!]} grading] (set-value! (get adj key))))}))

(defn- body-for [tab]
  (case tab
    :basic   (basic-body)
    (:detail :look) (basic-like-body tab)
    :color   (color-body)
    :local   (local-body)
    :spots   (spots-body)
    :crop    (crop-body)
    :curve   (curve-body)
    :presets (presets-body)
    :history (history-body)
    :info    (info-body)))

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
        clip-lbl (w/tlabel "" :normal "sys-10" "faint")
        rgb-btn  (doto (w/pill "RGB" (fn [] (swap! st/state update :hist-rgb not)) "sm") (w/set-base-a11y! "Show red, green and blue in the histogram"))
        clip-btn (doto (w/pill "CLIP" (fn [] (swap! st/state update :clip-view not)) "sm") (w/set-base-a11y! "Show clipped pixels on the photo (J)"))
        overlay  (crop-overlay/create iv)
        local-ov (local-overlay/create iv)
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
    (.setOnMouseClicked iv (w/handler (fn [e] (when (= :wb (:pick @st/state)) (pick-wb! e iv)))))
    (.bind (.fitWidthProperty iv) (.subtract (.widthProperty canvas) 40))
    (.bind (.fitHeightProperty iv) (.subtract (.heightProperty canvas) 40))
    (w/classes! canvas "ground")
    (w/add! canvas iv (:node overlay) (:node local-ov) loading)
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
                (doto (w/vbox 8 (w/hbox 8 (w/tlabel "HISTOGRAM" :wide "sys" "faint") (w/spacer) rgb-btn clip-btn)
                              (:node hist) clip-lbl)
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
             :update-histogram!
             (fn [h]
               ((:update! hist) h)
               (let [[lo hi] (histogram/clip-percentages h)]
                 (.setText clip-lbl (theme/tracked (format "SHADOWS %.1f%% · HIGHLIGHTS %.1f%%" lo hi) :normal))
                 (w/a11y! clip-lbl (format "Clipped: %.1f percent of pixels in the shadows, %.1f percent in the highlights" lo hi))))
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
                 ((:refresh! local-ov) s)
                 ((:set-mode! hist) (if (:hist-rgb s) :rgb :luma))
                 (w/set-on! rgb-btn (boolean (:hist-rgb s)))
                 (w/set-on! clip-btn (boolean (:clip-view s)))
                 (.setCursor iv (if (:pick s) javafx.scene.Cursor/CROSSHAIR javafx.scene.Cursor/DEFAULT))
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
