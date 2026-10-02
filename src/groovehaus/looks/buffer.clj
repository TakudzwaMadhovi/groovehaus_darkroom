(ns groovehaus.looks.buffer
  "Float RGB working buffers for the Looks pipeline.

  A buffer is {:w :h :rgb :alpha}: `rgb` is a float[] of interleaved sRGB-encoded
  values in [0,1], `alpha` a byte[] (one per pixel, carried through untouched).
  Buffers returned from public fns are treated as immutable; a look's render fn
  only mutates the private copy the pipeline hands it."
  (:import [java.awt.image BufferedImage]
           [java.util.function IntConsumer]
           [java.util.stream IntStream]))

(set! *warn-on-reflection* true)

(defn par-rows
  "Calls (f y) for each y in [0,h), spread over the common fork-join pool."
  [^long h f]
  (let [^IntStream s (.parallel (IntStream/range 0 h))]
    (.forEach s (reify IntConsumer (accept [_ y] (f (long y)))))))

(def ^:private ^:const inv255 (/ 1.0 255.0))

(defn ->buffer
  "Decodes a BufferedImage (any type) into a float working buffer."
  [^BufferedImage img]
  (let [w (.getWidth img) h (.getHeight img) n (* w h)
        ^ints px (.getRGB img 0 0 w h nil 0 w)
        ^floats rgb (float-array (* 3 n))
        ^bytes alpha (byte-array n)]
    (par-rows h
      (fn [^long y]
        (dotimes [x w]
          (let [i (+ (* y w) x)
                p (aget px i)
                j (* 3 i)]
            (aset alpha i (unchecked-byte (bit-and (bit-shift-right p 24) 0xff)))
            (aset rgb j (float (* inv255 (bit-and (bit-shift-right p 16) 0xff))))
            (aset rgb (+ j 1) (float (* inv255 (bit-and (bit-shift-right p 8) 0xff))))
            (aset rgb (+ j 2) (float (* inv255 (bit-and p 0xff))))))))
    {:w w :h h :rgb rgb :alpha alpha}))

(defn- ->byte ^long [^double v]
  (long (+ 0.5 (* 255.0 (Math/max 0.0 (Math/min 1.0 v))))))

(defn ->image
  "Encodes a working buffer back to a TYPE_INT_ARGB BufferedImage."
  ^BufferedImage [{:keys [^long w ^long h ^floats rgb ^bytes alpha]}]
  (let [img (BufferedImage. (int w) (int h) BufferedImage/TYPE_INT_ARGB)
        px (int-array (* w h))]
    (par-rows h
      (fn [^long y]
        (dotimes [x w]
          (let [i (+ (* y w) x)
                j (* 3 i)
                a (bit-and (long (aget alpha i)) 0xff)
                r (->byte (aget rgb j))
                g (->byte (aget rgb (+ j 1)))
                b (->byte (aget rgb (+ j 2)))]
            (aset px i (unchecked-int (bit-or (bit-shift-left a 24) (bit-shift-left r 16)
                                              (bit-shift-left g 8) b)))))))
    (.setRGB img 0 0 (int w) (int h) px 0 (int w))
    img))

(defn copy-buffer
  "Independent copy of the pixel data (alpha is shared; nothing mutates it)."
  [buf]
  (assoc buf :rgb (aclone ^floats (:rgb buf))))
