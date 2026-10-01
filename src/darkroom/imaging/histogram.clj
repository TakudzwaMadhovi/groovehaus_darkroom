(ns darkroom.imaging.histogram
  "Colour channel distribution of an image. Pure logic, no UI dependency.")

(set! *unchecked-math* :warn-on-boxed)

(defn compute
  "Counts pixels per intensity (0-255) for each channel.
  Returns {:r :g :b :luma :count} where each channel is a long-array of 256
  bins and :count is the total number of pixels. Luma uses Rec. 709 weights,
  matching adjust-saturation."
  [{:keys [pixels]}]
  (let [^ints src pixels
        ^longs r (long-array 256)
        ^longs g (long-array 256)
        ^longs b (long-array 256)
        ^longs l (long-array 256)
        n (alength src)]
    (loop [i 0]
      (when (< i n)
        (let [p  (aget src i)
              pr (bit-and (unsigned-bit-shift-right p 16) 0xFF)
              pg (bit-and (unsigned-bit-shift-right p 8) 0xFF)
              pb (bit-and p 0xFF)
              y  (bit-shift-right (+ (* 54 pr) (* 183 pg) (* 19 pb)) 8)]
          (aset r pr (inc (aget r pr)))
          (aset g pg (inc (aget g pg)))
          (aset b pb (inc (aget b pb)))
          (aset l y (inc (aget l y))))
        (recur (inc i))))
    {:r r :g g :b b :luma l :count n}))
