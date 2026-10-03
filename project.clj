(def linux-arm64?
  (let [os   (System/getProperty "os.name" "")
        arch (System/getProperty "os.arch" "")]
    (and (not (re-find #"(?i)mac|win" os))
         (contains? #{"aarch64" "arm64"} arch))))

;; JavaFX 21.x has no linux-aarch64 build on Maven Central (its "linux" jar is
;; x86_64 only); 22.0.2 is the oldest release that does, and runs on JDK 17+.
(def javafx-version (if linux-arm64? "22.0.2" "21.0.5"))

;; JavaFX artifacts ship per-platform natives, selected by Maven classifier.
(def javafx-platform
  (let [os   (System/getProperty "os.name" "")
        arch (System/getProperty "os.arch" "")
        arm? (contains? #{"aarch64" "arm64"} arch)]
    (cond
      (re-find #"(?i)mac" os) (if arm? "mac-aarch64" "mac")
      (re-find #"(?i)win" os) "win"
      :else                   (if arm? "linux-aarch64" "linux"))))

;; Bytedeco (JavaCPP) ships natives per platform, selected by classifier. OpenCV
;; (denoising) and OpenBLAS have linux-arm64 builds; LibRaw does not, so there RAW
;; files are decoded by LibRaw's dcraw_emu tool instead (darkroom.imaging.raw-cli).
(def bytedeco-version "1.5.11")
(def libraw-version (str "0.21.2-" bytedeco-version))
(def opencv-version (str "4.10.0-" bytedeco-version))
(def openblas-version (str "0.3.28-" bytedeco-version))

(def bytedeco-platform
  (let [os   (System/getProperty "os.name" "")
        arch (System/getProperty "os.arch" "")
        arm? (contains? #{"aarch64" "arm64"} arch)]
    (cond
      (re-find #"(?i)mac" os) (if arm? "macosx-arm64" "macosx-x86_64")
      (re-find #"(?i)win" os) "windows-x86_64"
      :else                   (if arm? "linux-arm64" "linux-x86_64"))))

(def libraw-native?
  (not= "linux-arm64" bytedeco-platform))

(def dependencies
  (cond-> [['org.clojure/clojure "1.12.0"]
           ['org.openjfx/javafx-base javafx-version :classifier javafx-platform]
           ['org.openjfx/javafx-graphics javafx-version :classifier javafx-platform]
           ['org.openjfx/javafx-controls javafx-version :classifier javafx-platform]
           ['com.drewnoakes/metadata-extractor "2.19.0"] ; EXIF orientation
           ['com.google.zxing/core "3.5.3"]               ; QR code for the phone companion address
           ['org.bytedeco/javacpp bytedeco-version]
           ['org.bytedeco/javacpp bytedeco-version :classifier bytedeco-platform]
           ['org.bytedeco/opencv opencv-version]
           ['org.bytedeco/opencv opencv-version :classifier bytedeco-platform]
           ;; OpenCV's native library links against OpenBLAS.
           ['org.bytedeco/openblas openblas-version :classifier bytedeco-platform]
           ['org.bytedeco/libraw libraw-version]]
    libraw-native? (conj ['org.bytedeco/libraw libraw-version :classifier bytedeco-platform])))

(defproject groovehaus-darkroom "0.1.0-SNAPSHOT"
  :description "Desktop image processing application (Clojure + JavaFX)"
  :min-lein-version "2.9.0"
  :dependencies ~dependencies
  :main darkroom.main
  :resource-paths ["resources"]
  ;; headless AWT: ImageIO needs no Cocoa/AWT event loop, which can conflict
  ;; with JavaFX on macOS. The float pipeline holds 12 bytes per pixel per image
  ;; (a 45 MP export needs about 1.7 GB), so the heap scales with the machine
  ;; instead of a fixed cap.
  :jvm-opts ["-Djava.awt.headless=true" "-XX:MaxRAMPercentage=60"]
  :global-vars {*warn-on-reflection* true}
  :profiles {:uberjar {:aot :all}
             ;; test helpers use plain interop for brevity
             :test   {:global-vars {*warn-on-reflection* false}}})
