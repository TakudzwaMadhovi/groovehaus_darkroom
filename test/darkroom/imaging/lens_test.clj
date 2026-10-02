(ns darkroom.imaging.lens-test
  (:require [clojure.test :refer [deftest is testing]]
            [darkroom.imaging.geometry :as geometry]
            [darkroom.imaging.lens :as lens]
            [darkroom.imaging.scene :as scene]))

(def ^:private xml
  (.getBytes "<?xml version=\"1.0\"?>
<!DOCTYPE lensdatabase SYSTEM \"lensfun-database.dtd\">
<lensdatabase version=\"2\">
  <lens>
    <maker>Nikon</maker>
    <model>Nikkor 50mm f/1.8</model>
    <mount>Nikon F AF</mount>
    <cropfactor>1</cropfactor>
    <calibration>
      <distortion model=\"poly3\" focal=\"50\" k1=\"-0.02\" />
    </calibration>
  </lens>
  <lens>
    <maker>Nikon</maker>
    <model>Nikkor AF-S 50mm f/1.8G</model>
    <cropfactor>1</cropfactor>
    <calibration>
      <distortion model=\"ptlens\" focal=\"50\" a=\"0.01\" b=\"-0.03\" c=\"0.005\" />
    </calibration>
  </lens>
  <lens>
    <maker>Sigma</maker>
    <model>Zoom 18-35mm f/1.8</model>
    <cropfactor>1.5</cropfactor>
    <calibration>
      <distortion model=\"poly5\" focal=\"18\" k1=\"0.10\" k2=\"-0.02\" real-focal=\"18\" />
      <distortion model=\"poly5\" focal=\"35\" k1=\"-0.02\" k2=\"0.01\" />
      <tca model=\"linear\" focal=\"18\" kr=\"1.0004\" kb=\"0.9996\" />
      <tca model=\"linear\" focal=\"35\" kr=\"1.0000\" kb=\"1.0000\" />
    </calibration>
  </lens>
  <lens>
    <maker>Nobody</maker>
    <model>Uncalibrated</model>
    <cropfactor>1</cropfactor>
  </lens>
</lensdatabase>" "UTF-8"))

(def ^:private db (lens/parse-database xml))

(defn- near? [a b] (< (Math/abs (- (double a) (double b))) 1e-6))

(deftest parses-a-lensfun-file-with-its-doctype
  (is (= 3 (count db)) "the lens without a calibration is left out")
  (let [e (first (:distortion (first (:calibrations (first db)))))]
    (is (= [:poly3 [-0.02] 50.0] [(:model e) (:terms e) (:focal e)]))
    (is (near? 51.0 (:real-focal e)) "default real focal length of poly3 is focal * (1 - k1)"))
  (is (= [18.0 35.0] (map :focal (:distortion (first (:calibrations (nth db 2)))))))
  (is (= [1.5 1.5] ((juxt :crop :aspect) (first (:calibrations (nth db 2)))))
      "aspect defaults to 3:2")
  (testing "an external entity is never read"
    (let [evil (lens/parse-database
                 (.getBytes (str "<!DOCTYPE x [<!ENTITY a SYSTEM \"file:///etc/passwd\">]><lensdatabase><lens><model>&a;</model>"
                                 "<cropfactor>1</cropfactor><calibration><distortion model=\"poly3\" focal=\"50\" k1=\"0\"/></calibration>"
                                 "</lens></lensdatabase>") "UTF-8"))]
      (is (not-any? #(re-find #"root:" (str (:model %))) evil)))))

(deftest finds-the-most-specific-lens
  (is (= "Nikkor AF-S 50mm f/1.8G" (:model (lens/find-lens db {:lens-model "Nikkor AF-S 50mm f/1.8G"}))))
  (is (= 1 (count (lens/find-lenses db {:lens-model "Nikkor 50mm f/1.8"}))) "the plain 50mm is not tied with the G")
  (is (= "Nikkor 50mm f/1.8" (:model (lens/find-lens db {:lens-model "Nikon Nikkor 50mm f/1.8"}))))
  (is (nil? (lens/find-lens db {:lens-model "Canon EF 85mm f/1.4"})))
  (is (nil? (lens/find-lens db {}))))

(deftest rescales-the-calibration-into-focal-length-units
  (testing "poly3 (hand-worked): real focal 51, Hugin half-height 12 mm, d = 1 - k1"
    (let [p (lens/profile (first db) 50.0 1.0)]
      (is (= :poly3 (:model p)))
      (is (near? -0.34041394335 (first (:terms p))))
      (is (near? 0.84836500599 (:unit p)))))
  (testing "ptlens: a' = a hs^3/d^4, b' = b hs^2/d^3, c' = c hs/d^2"
    (let [p (lens/profile (second db) 50.0 1.0)]
      (is (every? true? (map near? [0.71268929 -0.51313625 0.02052545] (:terms p))))
      (is (near? 0.85254416 (:unit p)))))
  (testing "a camera with a smaller sensor than the calibration has no profile"
    (is (nil? (lens/profile (first db) 50.0 0.5)))))

(deftest interpolates-like-lensfun
  (let [l (nth db 2)]
    (testing "exact focal length: the calibration itself"
      (is (near? 18.0 (/ 43.2666153056 1.5 (:unit (lens/profile l 18.0 1.5)))) "real focal length 18"))
    (testing "halfway between two calibrations (two points: linear on term * focal, then / focal)"
      (let [p (lens/profile l 26.5 1.5)]
        (is (= :poly5 (:model p)))
        (is (near? 0.22773437 (first (:terms p))))
        (is (near? -0.02271683 (second (:terms p))))
        (is (near? 1.08846831 (:unit p)))
        (is (near? 1.0002 (first (:ca p))))
        (is (near? 0.9998 (second (:ca p))))))
    (testing "outside the calibrated range the nearest calibration is used"
      (is (= (:terms (lens/profile l 10.0 1.5)) (:terms (lens/profile l 18.0 1.5)))))))

(deftest hermite-spline-through-three-calibrations
  (let [xml3 (.getBytes "<lensdatabase><lens><model>Z</model><cropfactor>1</cropfactor><calibration>
      <distortion model=\"poly5\" focal=\"10\" k1=\"0.1\" k2=\"0\" real-focal=\"10\"/>
      <distortion model=\"poly5\" focal=\"20\" k1=\"0.2\" k2=\"0\" real-focal=\"20\"/>
      <distortion model=\"poly5\" focal=\"40\" k1=\"0.5\" k2=\"0\" real-focal=\"40\"/>
      </calibration></lens></lensdatabase>" "UTF-8")
        l (first (lens/parse-database xml3))
        ;; unit = D / crop / real focal: real focal interpolates through (10 10) (20 20) (40 40) = the line, so real = focal
        p (lens/profile l 30.0 1.0)
        real (/ 43.2666153056 (:unit p))]
    ;; real focal length: Hermite through (20, 20), (40, 40) with neighbours (10, 10): tg2 = 15, tg3 = 20
    ;; -> 0.5*20 + 0.125*15 + 0.5*40 - 0.125*20 = 29.375 (the spline is not exactly linear on uneven spacing)
    (is (near? 29.375 real))
    ;; scaled values y = k1 * f: 1, 4, 20 at f = 10, 20, 40; at 30 (t = 0.5 between 20 and 40)
    ;; tangents: tg2 = (y3 - y1)/2 = (20 - 1)/2 = 9.5, tg3 = y3 - y2 = 16 (no fourth point)
    ;; hermite(0.5) = 0.5*4 + 0.125*9.5 + 0.5*20 - 0.125*16 = 2 + 1.1875 + 10 - 2 = 11.1875 -> k1 = 11.1875 / 30
    (let [k1 (/ 11.1875 30.0)
          hs (/ 29.375 (/ 43.2666153056 (Math/hypot 1.5 1.0) 2.0))]
      (is (near? (* k1 hs hs) (first (:terms p)))))))

(deftest profile-for-a-picture
  (let [tags {:lens-model "Nikkor 50mm f/1.8" :focal-length [50 1] :focal-length-35mm 50}
        p (lens/profile-for db tags)]
    (is (= :poly3 (:model p)))
    (is (near? 0.84836500599 (:unit p))))
  (testing "a cropped sensor shows in the 35mm-equivalent focal length"
    (is (near? 1.5 (lens/crop-factor {:focal-length [50 1] :focal-length-35mm 75} 1.0))))
  (is (nil? (lens/profile-for db {:lens-model "Nikkor 50mm f/1.8"})) "no focal length, no profile"))

(defn- ramp
  "A scene image whose red channel is the horizontal distance from the centre in pixels."
  [w h]
  (let [a (float-array (* 3 w h))]
    (dotimes [y h] (dotimes [x w] (aset a (* 3 (+ (* y w) x)) (float (- (+ x 0.5) (* 0.5 w))))))
    (scene/image w h a)))

(deftest the-geometry-follows-the-profile-formula
  (let [w 400 h 300
        unit 60.0 k1 0.05
        p {:model :poly3 :terms [k1] :unit unit}
        out (geometry/geometry (ramp w h) {:lens-profile p})
        ;; output may be zoomed to fill: recover the zoom from the pixel at x = 0 (centre) and use the formula at another
        z (geometry/zoom-for {:lens-profile p} w h)
        scale (/ unit (Math/hypot w h))
        px (fn [x] (aget ^floats (:data out) (* 3 (+ (* (quot h 2) w) x))))]
    (is (> z 1.0) "pincushion-corrected frame is zoomed to stay filled")
    (doseq [x [250 300 350]]
      (let [xo (- (+ x 0.5) (* 0.5 w))          ; output offset from centre, px
            xu (/ xo z)                          ; before the fill zoom
            rho (* scale (Math/abs xu))
            expect (* xu (+ 1.0 (* k1 rho rho)))]
        ;; row quot h 2 is half a pixel off the centre line; the ramp is horizontal only
        (is (< (Math/abs (- (px x) expect)) 0.6) (str "x=" x))))
    (testing "no profile: nothing changes"
      (let [i (ramp 4 4)] (is (identical? i (geometry/geometry i {:lens-profile nil})))))))

(deftest chromatic-aberration-from-the-profile
  (let [w 200 h 100
        a (float-array (* 3 w h))]
    (dotimes [y h] (dotimes [x w] (dotimes [c 3] (aset a (+ (* 3 (+ (* y w) x)) c) (float (- (+ x 0.5) (* 0.5 w)))))))
    (let [out (geometry/geometry (scene/image w h a) {:lens-profile {:model :poly3 :terms [0.0] :unit 40.0 :ca [1.01 0.99]}})
          at (fn [c x] (aget ^floats (:data out) (+ (* 3 (+ (* 50 w) x)) c)))
          x 180]
      (is (> (at 0 x) (at 1 x)) "red samples further from the centre")
      (is (< (at 2 x) (at 1 x)) "blue samples nearer"))))
