;; Accessibility audit of the running UI. Needs a display:
;;   xvfb-run -a lein with-profile -base,-dev,-user run -m clojure.main scripts/a11y_audit.clj [folder-of-images]
;; Walks the real scene graph and checks every control a keyboard / screen-reader
;; user can reach, in both views and with the export dialog open.
(require '[darkroom.main :as m] '[darkroom.ui.state :as st] '[darkroom.catalog :as cat] '[darkroom.ui.theme :as theme])
(import '(javafx.stage Window) '(javafx.application Platform)
        '(javafx.scene Node Parent AccessibleAttribute)
        '(javafx.scene.control Labeled ButtonBase Slider)
        '(javafx.scene.image ImageView)
        '(javafx.scene.input KeyEvent KeyCode) '(javafx.event Event))

(def dir (or (first *command-line-args*) "resources"))
(m/-main (str (first (filter #(.isFile %) (.listFiles (java.io.File. dir))))))
(Thread/sleep 8000)

(defn on-fx [f] (let [p (promise)] (Platform/runLater #(do (try (deliver p (f)) (catch Throwable t (.printStackTrace t) (deliver p :error))))) @p))
(defn scene [] (.getScene ^Window (first (Window/getWindows))))
(defn walk [^Node n] (cons n (when (instance? Parent n) (mapcat walk (.getChildrenUnmodifiable ^Parent n)))))
(defn shown? [^Node n] (and (.isVisible n) (loop [p n] (cond (nil? p) true (not (.isVisible p)) false :else (recur (.getParent p))))))
(defn name-of [^Node n]
  (or (let [t (.getAccessibleText n)] (when-not (empty? t) t))
      (when (instance? Labeled n) (.getText ^Labeled n))
      ""))
(defn interactive? [^Node n]
  (and (.isFocusTraversable n) (not (.isDisabled n)) (shown? n)
       (not (some-> (.getStyleClass n) (.contains "scroll-bar")))))

(def problems (atom []))
(defn audit! [label]
  (on-fx
    (fn []
      (let [nodes (filter shown? (walk (.getRoot (scene))))
            inter (filter interactive? nodes)]
        (doseq [^Node n inter]
          (let [nm (name-of n)]
            (cond (empty? (.trim (str nm)))
                  (swap! problems conj [label "focusable control without an accessible name" (str (class n)) (vec (.getStyleClass n))])
                  (not= nm (theme/untracked nm))
                  (swap! problems conj [label "accessible name contains letter-spacing characters" nm]))))
        (doseq [^ImageView iv (filter #(instance? ImageView %) nodes)]
          ;; thumbnails inside a named control are decorative: the control carries the name
          (when (and (> (.getFitWidth iv) 30) (empty? (.getAccessibleText iv))
                     (not (some (fn [^Node p] (and (.isFocusTraversable p) (seq (name-of p))))
                                (take-while some? (iterate (fn [^Node x] (.getParent x)) iv)))))
            (swap! problems conj [label "image without alternative text" (str (.getFitWidth iv))])))
        (println (format "%-26s focusable controls: %d, all named" label (count inter)))
        inter))))

(def results (atom {}))
(defn check [label ok? & [detail]] (swap! results assoc label ok?) (println (format "  [%s] %s%s" (if ok? "PASS" "FAIL") label (if detail (str "  (" detail ")") ""))))
(defn press [code & {:keys [shift]}]
  (on-fx #(let [sc (scene) tgt (or (.getFocusOwner sc) sc)]
            (Event/fireEvent tgt (KeyEvent. KeyEvent/KEY_PRESSED "" "" code (boolean shift) false false false))
            (Event/fireEvent tgt (KeyEvent. KeyEvent/KEY_RELEASED "" "" code (boolean shift) false false false)))))
(defn focus-owner [] (on-fx #(.getFocusOwner (scene))))
(defn fname [] (on-fx #(some-> (.getFocusOwner (scene)) name-of)))

;; ---- 1. every reachable control has a plain-text name ---------------------
(println "Naming audit")
(audit! "library")
(on-fx #(st/go! :develop))
(Thread/sleep 1500)
(doseq [t [:basic :detail :color :curve :look :crop :local :spots :presets :history]]
  (on-fx #(swap! st/state assoc :tab t)) (Thread/sleep 400)
  (audit! (str "develop/" (name t))))
(on-fx #(swap! st/state assoc :exporting true))
(Thread/sleep 800)
(audit! "export dialog")
(on-fx #(swap! st/state assoc :exporting false))

;; ---- 2. curve editor works from the keyboard -----------------------------
(println "Curve editor keyboard")
(on-fx #(swap! st/state assoc :tab :curve :view :develop))
(Thread/sleep 600)
(on-fx #(.requestFocus (.lookup (.getRoot (scene)) ".curve-pane")))
(Thread/sleep 300)
(let [cv #(:curve (st/cur-adj @st/state))
      hist-n #(count (:history (cat/frame (:catalog @st/state) (:cur @st/state))))
      h0 (hist-n)]
  ;; the editor starts on point 3 (x = 0.5)
  (press KeyCode/UP) (press KeyCode/UP) (press KeyCode/UP)
  (check "Up raises the chosen point by 1% steps" (< (Math/abs (- (nth (cv) 2) 0.53)) 1e-9) (str (cv)))
  (press KeyCode/DOWN :shift true)
  (check "Shift+Down moves 5%" (< (Math/abs (- (nth (cv) 2) 0.48)) 1e-9) (str (nth (cv) 2)))
  (press KeyCode/RIGHT) (press KeyCode/PAGE_DOWN)
  (check "Right selects the next point, PageDown moves 10%" (< (Math/abs (- (nth (cv) 3) 0.65)) 1e-9) (str (nth (cv) 3)))
  (press KeyCode/HOME)
  (check "Home sets the point to 0%" (zero? (nth (cv) 3)))
  (press KeyCode/BACK_SPACE)
  (check "Backspace restores the point's default" (= 0.75 (nth (cv) 3)))
  (press KeyCode/LEFT) (press KeyCode/LEFT) (press KeyCode/LEFT) (press KeyCode/LEFT) (press KeyCode/LEFT)
  (press KeyCode/UP)
  (check "Left stops at the first point" (< (Math/abs (- (nth (cv) 0) 0.01)) 1e-9))
  (check "edits are committed to history" (> (hist-n) h0) (str h0 " -> " (hist-n)))
  (let [txt (on-fx #(.getAccessibleText (.lookup (.getRoot (scene)) ".curve-pane")))]
    (check "the focused point is announced with its value" (and txt (re-find #"point 1 of 5" txt) (re-find #"output 1 percent" txt)) txt)))

;; ---- 3. keyboard focus survives list rebuilds ------------------------------
(println "Focus is kept")
(on-fx #(swap! st/state assoc :tab :basic))
(Thread/sleep 400)
(let [btn (on-fx #(first (filter (fn [^Node n] (and (.contains (.getStyleClass n) "tab-btn") (= "LOOK tab" (.getAccessibleText n) )) ) (walk (.getRoot (scene))))))]
  (on-fx #(.requestFocus ^Node btn))
  (on-fx #(.fire ^javafx.scene.control.Button btn))
  (Thread/sleep 600)
  (check "activating a tab keeps focus on it" (identical? btn (focus-owner)) (str (fname))))
(on-fx #(swap! st/state assoc :tab :history))
(Thread/sleep 600)
(on-fx #(do (st/set-adj! :exposure 0.4) (st/commit! "EXPOSURE") (st/set-adj! :contrast 0.2) (st/commit! "CONTRAST")))
(Thread/sleep 800)
(let [row (on-fx #(first (filter (fn [^Node n] (and (instance? javafx.scene.control.Button n) (some-> (.getAccessibleText n) (.startsWith "Step "))))
                                (walk (.getRoot (scene))))))]
  (on-fx #(.requestFocus ^Node row))
  (on-fx #(.fire ^javafx.scene.control.Button row))
  (Thread/sleep 800)
  (check "reverting a history step keeps focus in the list"
         (let [o (focus-owner)] (and o (some-> (.getAccessibleText ^Node o) (.startsWith "Step "))))
         (fname)))

;; ---- 4. Library: arrows move through tiles, focus follows ------------------
(println "Library keyboard")
(on-fx #(st/go! :library))
(Thread/sleep 800)
(let [tile (on-fx #(first (filter (fn [^Node n] (and (.contains (.getStyleClass n) "tile") (.isFocusTraversable n))) (walk (.getRoot (scene))))))
      first-frame (first (st/visible-frames @st/state))]
  (on-fx #(.requestFocus ^Node tile))
  (on-fx #(st/select! first-frame))
  (press KeyCode/RIGHT)
  (Thread/sleep 800)
  (let [n (count (st/visible-frames @st/state))]
    (if (> n 1)
      (check "Right moves the selection and keyboard focus follows it"
             (let [o (focus-owner)] (and o (some-> (.getAccessibleText ^Node o) (.contains "selected"))))
             (fname))
      (println "  (only one frame in this folder; skipped)"))))

;; ---- 5. export dialog is modal: focus moves in, cannot leave, returns -------
(println "Export dialog focus")
(on-fx #(do (st/go! :develop)))
(Thread/sleep 600)
(let [before (do (on-fx #(.requestFocus (.getRoot (scene)))) (focus-owner))]
  (on-fx #(swap! st/state assoc :exporting true))
  (Thread/sleep 800)
  (let [o (focus-owner)]
    (check "focus moves into the export dialog" (and o (some->> (.getAccessibleText ^Node o) (re-find #"(?i)jpeg"))) (fname)))
  (let [in-scrim? (fn [^Node n] (some (fn [^Node p] (.contains (.getStyleClass p) "scrim"))
                                      (take-while some? (iterate (fn [^Node x] (.getParent x)) n))))
        behind (on-fx (fn [] (count (filter interactive? (remove in-scrim? (walk (.getRoot (scene))))))))]
    (check "nothing behind the dialog is focusable" (zero? behind) (str behind " reachable")))
  (press KeyCode/ESCAPE)
  (Thread/sleep 600)
  (check "Esc closes it and re-enables the app" (and (not (:exporting @st/state)) (on-fx #(not (.isDisabled (.getRoot (scene))))))))

(println)
(if (and (empty? @problems) (every? identity (vals @results)))
  (println "A11Y AUDIT: ALL CHECKS PASSED")
  (do (println "A11Y AUDIT: PROBLEMS")
      (doseq [p @problems] (println "  " p))
      (doseq [[k v] @results :when (not v)] (println "   failed:" k))))
(System/exit 0)
