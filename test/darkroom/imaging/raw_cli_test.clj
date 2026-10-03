(ns darkroom.imaging.raw-cli-test
  "The dcraw_emu fallback (used where LibRaw natives are unavailable) must agree with
  the native decoder. Skipped when dcraw_emu is not installed."
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.dng :refer [write-dng!]]
            [darkroom.imaging.raw :as raw]
            [darkroom.imaging.raw-cli :as cli]))

(defn- max-diff [^shorts a ^shorts b]
  (reduce max (map (fn [i] (Math/abs (- (bit-and (aget a i) 0xFFFF) (bit-and (aget b i) 0xFFFF))))
                   (range (alength a)))))

(deftest cli-decode-matches-native
  (when (and (cli/available?) (raw/native-available?))
    (let [f   (write-dng! 64 48 [20000 30000 12000])
          nat (#'raw/decode-native f {:output-color 4})
          cl  (cli/decode-linear f {:output-color 4})]
      (is (= (select-keys nat [:width :height :channels :bits])
             (select-keys cl  [:width :height :channels :bits])))
      (is (< (max-diff (:data nat) (:data cl)) 8)))))

(deftest cli-half-size-and-multipliers
  (when (cli/available?)
    (let [f  (write-dng! 64 48 16384)
          h  (cli/decode-linear f {:half-size? true :quality 0 :multipliers? true})]
      (is (= [32 24] [(:width h) (:height h)]))
      (is (= 3 (count (:multipliers h))))
      (is (every? #(< 0.9 % 1.1) (:multipliers h))))))

(deftest cli-reports-unreadable-files
  (when (cli/available?)
    (let [f (java.io.File/createTempFile "not-a-raw" ".dng")]
      (.deleteOnExit f)
      (spit f "nope")
      (is (thrown? clojure.lang.ExceptionInfo (cli/decode-linear f))))))

(deftest cli-embedded-thumbnail-matches-native
  (when (and (cli/available?) (raw/native-available?))
    (let [jpg (let [bi (java.awt.image.BufferedImage. 120 80 java.awt.image.BufferedImage/TYPE_INT_RGB)
                    o (java.io.ByteArrayOutputStream.)]
                (javax.imageio.ImageIO/write bi "jpg" o)
                (.toByteArray o))
          f   (write-dng! 64 48 20000 {:thumb jpg :thumb-size [120 80] :orientation 6})
          n   (#'raw/embedded-thumbnail-native f 64)
          c   (cli/embedded-thumbnail f 64)]
      (is (some? n))
      (is (= [(:width n) (:height n)] [(:width c) (:height c)]))
      (testing "too small a preview is rejected"
        (is (nil? (cli/embedded-thumbnail f 400)))))))
