(ns mark.draw
  (:require
    [mark.fundaments :refer :all]
    [mark.protocols :as protocols]
    [mark.color :as color]
    [clojure.java.io :as io])
  (:import
    [java.awt Polygon BasicStroke Font Graphics2D Color RenderingHints Rectangle]
    [java.awt.font TextLayout]
    [java.awt.geom Area AffineTransform Rectangle2D$Double Ellipse2D$Double]
    [javax.imageio ImageIO]
    [java.awt.image BufferedImage RenderedImage]
    [java.io File]))



(def font-regular (Font/createFont Font/TRUETYPE_FONT
                                   (io/input-stream (io/resource "fonts/ah/AtkinsonHyperlegible-Regular.ttf"))))
(def font-bold (Font/createFont Font/TRUETYPE_FONT
                                (io/input-stream (io/resource "fonts/ah/AtkinsonHyperlegible-Bold.ttf"))))

(extend-protocol
  protocols/Style
  nil
  (prepare-for-draw [_ _] false)
  (prepare-for-fill [_ _] false))

(def default-style
  (reify protocols/Style
    (prepare-for-draw [_ g]
      (.setColor g Color/BLACK)
      (.setStroke g (BasicStroke. 1))
      (.setFont g font-regular)
      true)
    (prepare-for-fill [_ _]
      false)))

(defn styles [& styles]
  (reify protocols/Style
    (prepare-for-draw [_ g]
      (some identity (mapv #(protocols/prepare-for-draw % g) (cons default-style styles))))
    (prepare-for-fill [_ g]
      (some identity (mapv #(protocols/prepare-for-fill % g) (cons default-style styles))))))

(defn grayscale [style]
  (reify protocols/Style
    (prepare-for-draw [_ g]
      (let [r (protocols/prepare-for-draw style g)]
        (.setColor g (color/grayify (.getColor g)))
        r))
    (prepare-for-fill [_ g]
      (let [r (protocols/prepare-for-fill style g)]
        (.setColor g (color/grayify (.getColor g)))
        r))))

(defn line-style
  ([] (line-style 1))
  ([width] (line-style width Color/BLACK))
  ([width color] (line-style width color BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))
  ([width color cap join]
   (reify protocols/Style
     (prepare-for-draw [_ g]
       (.setPaint g color)
       (.setStroke g (BasicStroke. width cap join))
       true)
     (prepare-for-fill [_ _]
       false))))

(defn fill-style [fill-paint]
  (reify protocols/Style
    (prepare-for-draw [_ _g]
      false)
    (prepare-for-fill [_ g]
      (.setPaint g fill-paint)
      true)))

(defn shape-style [stroke-color stroke-width fill-paint]
  (reify protocols/Style
    (prepare-for-draw [_ g]
      (.setPaint g stroke-color)
      (.setStroke g (BasicStroke. stroke-width BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))
      true)
    (prepare-for-fill [_ g]
      (.setPaint g fill-paint)
      true)))

(defn text-style
  ([font-size color] (text-style font-size color false))
  ([font-size color bold?]
   (reify protocols/Style
     (prepare-for-draw [_ g]
       (.setFont g (.deriveFont (if bold? font-bold font-regular) (float font-size)))
       (.setColor g color)
       true)
     (prepare-for-fill [_ _]
       false))))

(defn text-shape-style [font-size outline outline-width fill bold?]
  (reify protocols/Style
    (prepare-for-draw [_ g]
      (.setFont g (.deriveFont (if bold? font-bold font-regular) (float font-size)))
      (.setPaint g outline)
      (.setStroke g (BasicStroke. outline-width BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))
      true)
    (prepare-for-fill [_ g]
      (.setPaint g fill)
      true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defn p->sin [p]
;  (/ (- 1 (Math/cos (TAUS p))) 2))

;(defn for-points [points f]
;  (for [[idx [a b]] (map-indexed vector (partition 2 1 points))
;        :let [p (/ idx (count points))]]
;    (f a b p)))

;(defn do-points [points f]
;  (doseq [[idx [a b]] (map-indexed vector (partition 2 1 points))
;          :let [p (/ idx (count points))]]
;    (f a b p)))
;
;(defn lines [g points width-a width-b]
;  (do-points points
;    (fn [[ax ay] [bx by] p]
;      (.setStroke g (BasicStroke. (lerp width-a width-b p)))
;      (.drawLine g ax ay bx by))))
;
;
;
(defn line
  ([g style [[xa ya] [xb yb]]]
   (line g style xa ya xb yb))
  ([g style [xa ya] [xb yb]]
   (line g style xa ya xb yb))
  ([g style xa ya xb yb]
   (when (protocols/prepare-for-draw style g)
     (.drawLine g xa ya xb yb))))

(defn shape [^Graphics2D g style shape]
  (when (protocols/prepare-for-fill style g)
    (.fill g shape))
  (when (protocols/prepare-for-draw style g)
    (.draw g shape)))

(defmacro with-clip [g clip & body]
  `(let [pre-clip# (.getClip ~g)]
     (try
       (.setClip ~g ~clip)
       ~@body
       (finally
         (.setClip ~g pre-clip#)))))

(defmacro with-transform [g transform & body]
  `(let [pre-transform# (.getTransform ~g)]
     (try
       (.setTransform ~g ~transform)
       ~@body
       (finally
         (.setTransform ~g pre-transform#)))))

(defmacro with-new-image [[graphics-symbol image-form] & forms]
  `(let [image# ~image-form
         ~graphics-symbol (.getGraphics image#)]
     (.setRenderingHint ~graphics-symbol RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
     (.setRenderingHint ~graphics-symbol RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)
     ~@forms
     image#))

(defn new-image-file
  ([parent-dir image-name f-of-g]
   (new-image-file parent-dir image-name 200 200 f-of-g))
  ([parent-dir image-name width height f-of-g]
   (let [file (io/file parent-dir (str image-name ".png"))
         image (with-new-image [^Graphics2D g (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)]
                 (f-of-g g))]
     (io/make-parents file)
     (ImageIO/write ^RenderedImage image "png" ^File file)
     image)))

(defn instruction [graphics {this-shape :shape
                             this-style :style
                             this-line  :line
                             this-f     :f
                             :as _instruction}]
  (cond
    this-f (this-f graphics)
    this-line (line graphics this-style this-line) ; deprecated - make it a :f
    this-shape (shape graphics this-style this-shape) ; deprecated - make it a :f
    ))

(defn tx-instruction [rf]
  (fn
    ([] (rf))
    ([graphics] (rf graphics))
    ([graphics i]
     (rf graphics (instruction graphics i)))))

(defn tx-every [n fn-of-n]
  (fn [rf]
    (let [!n (volatile! 0)]
      (fn
        ([] (rf))
        ([acc] (rf acc))
        ([acc item]
         (when (zero? (mod (vswap! !n inc) n))
           (fn-of-n @!n))
         (rf acc item))))))

(defn rf-noop [init]
  (fn
    ([] init)
    ([value] value)
    ([value _] value)))

(defn instructions
  ([graphics ins]
   (instructions graphics ins identity))
  ([graphics ins tx]
   (transduce (comp tx tx-instruction) (rf-noop graphics) (remove nil? (sort-by :z-order ins))))
  #_(doseq [{this-shape :shape
           this-style :style
           this-line  :line
           this-f     :f
           :as i} (remove nil? (sort-by :z-order instructions))]
    ;(println i)
    (cond
      this-f (this-f graphics)
      this-line (line graphics this-style this-line)
      this-shape (shape graphics this-style this-shape))))
