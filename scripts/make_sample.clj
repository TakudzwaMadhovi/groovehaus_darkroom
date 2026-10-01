;; Regenerates resources/sample.png:  lein run -m clojure.main scripts/make_sample.clj
(import '(java.awt.image BufferedImage) '(java.awt Color GradientPaint RenderingHints) 'javax.imageio.ImageIO)
(let [w 800 h 600
      img (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
      g (.createGraphics img)]
  (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
  (.setPaint g (GradientPaint. 0 0 (Color. 40 60 120) w h (Color. 240 150 80)))
  (.fillRect g 0 0 w h)
  (doseq [[x y r c] [[220 260 140 (Color. 250 220 90)] [520 340 100 (Color. 90 200 170)] [640 160 70 (Color. 220 90 130)]]]
    (.setColor g c) (.fillOval g (- x r) (- y r) (* 2 r) (* 2 r)))
  (.dispose g)
  (ImageIO/write img "png" (java.io.File. "resources/sample.png")))
