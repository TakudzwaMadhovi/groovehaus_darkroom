(ns darkroom.imaging.histogram
  "Colour channel distribution and clipping of an image. Pure logic, no UI
  dependency.")

(set! *unchecked-math* :warn-on-boxed)

(defn compute
  "Counts pixels per intensity (0-255) for each channel.
  Returns {:r :g :b :luma :count :clip-low :clip-high}: each channel is a
  long-array of 256 bins, :count the total number of pixels, :clip-low and
  :clip-high the number of pixels with any channel at 0 / at 255 (the pixels
  the clipping warning marks). Luma uses Rec. 709 weights, matching
  adjust-saturation."
  [{:keys [pixels]}]
  (let [^ints src pixels
        ^longs r (long-array 256)
        ^longs g (long-array 256)
        ^longs b (long-array 256)
        ^longs l (long-array 256)
        n (alength src)]
    (loop [i 0 lo 0 hi 0]
      (if (< i n)
        (let [p  (aget src i)
              pr (bit-and (unsigned-bit-shift-right p 16) 0xFF)
              pg (bit-and (unsigned-bit-shift-right p 8) 0xFF)
              pb (bit-and p 0xFF)
              y  (bit-shift-right (+ (* 54 pr) (* 183 pg) (* 19 pb)) 8)]
          (aset r pr (inc (aget r pr)))
          (aset g pg (inc (aget g pg)))
          (aset b pb (inc (aget b pb)))
          (aset l y (inc (aget l y)))
          (recur (inc i)
                 (if (or (zero? pr) (zero? pg) (zero? pb)) (inc lo) lo)
                 (if (or (== 255 pr) (== 255 pg) (== 255 pb)) (inc hi) hi)))
        {:r r :g g :b b :luma l :count n :clip-low lo :clip-high hi}))))

(defn clip-percentages
  "[shadows highlights] clipped, as percentages of the pixels in `hist`."
  [{:keys [^long count ^long clip-low ^long clip-high]}]
  (if (pos? count)
    [(* 100.0 (/ (double clip-low) count)) (* 100.0 (/ (double clip-high) count))]
    [0.0 0.0]))

(def clip-high-colour (unchecked-int 0xFFFF2A2A))
(def clip-low-colour  (unchecked-int 0xFF2A5BFF))

(defn clipping-overlay
  "A copy of the image with clipped pixels painted: red where any channel is
  255 (highlights, which win), blue where any channel is 0 (shadows)."
  [{:keys [width height pixels]}]
  (let [^ints src pixels
        ^ints out (int-array (alength src))
        hi-c (int clip-high-colour)
        lo-c (int clip-low-colour)]
    (dotimes [i (alength src)]
      (let [p  (aget src i)
            pr (bit-and (unsigned-bit-shift-right p 16) 0xFF)
            pg (bit-and (unsigned-bit-shift-right p 8) 0xFF)
            pb (bit-and p 0xFF)]
        (aset out i (int (cond (or (== 255 pr) (== 255 pg) (== 255 pb)) hi-c
                               (or (zero? pr) (zero? pg) (zero? pb)) lo-c
                               :else p)))))
    {:width width :height height :pixels out}))
