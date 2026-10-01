(def javafx-version "21.0.5")

;; JavaFX artifacts ship per-platform natives, selected by Maven classifier.
(def javafx-platform
  (let [os   (System/getProperty "os.name" "")
        arch (System/getProperty "os.arch" "")
        arm? (contains? #{"aarch64" "arm64"} arch)]
    (cond
      (re-find #"(?i)mac" os) (if arm? "mac-aarch64" "mac")
      (re-find #"(?i)win" os) "win"
      :else                   (if arm? "linux-aarch64" "linux"))))

(defproject groovehaus-darkroom "0.1.0-SNAPSHOT"
  :description "Desktop image processing application (Clojure + JavaFX)"
  :min-lein-version "2.9.0"
  :dependencies [[org.clojure/clojure "1.12.0"]
                 ~['org.openjfx/javafx-base javafx-version :classifier javafx-platform]
                 ~['org.openjfx/javafx-graphics javafx-version :classifier javafx-platform]
                 ~['org.openjfx/javafx-controls javafx-version :classifier javafx-platform]]
  :main darkroom.main
  :resource-paths ["resources"]
  ;; headless AWT: ImageIO needs no Cocoa/AWT event loop, which can conflict
  ;; with JavaFX on macOS. Heap capped for 8 GB machines.
  :jvm-opts ["-Djava.awt.headless=true" "-Xmx2g"]
  :global-vars {*warn-on-reflection* true}
  :profiles {:uberjar {:aot :all}})
