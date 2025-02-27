(ns mark.shape
  (:require
    [mark.fundaments :refer :all]
    [mark.protocols :as protocols]
    [mark.props :as props])
  (:import
    [java.awt.geom Area AffineTransform Rectangle2D$Double Ellipse2D$Double]
    [java.awt Polygon Graphics2D]
    [java.awt.font TextLayout]))



(defn shape-intersect
  ([shape-a shape-b]
   (doto (Area. shape-a)
     (.intersect (Area. shape-b))))
  ([shape-a shape-b & more-shapes]
   (reduce shape-intersect (shape-intersect shape-a shape-b) more-shapes)))

(defn shape-subtract
  ([shape-a shape-b]
   (doto (Area. shape-a)
     (.subtract (Area. shape-b))))
  ([shape-a shape-b & more-shapes]
   (reduce shape-subtract (shape-subtract shape-a shape-b) more-shapes)))

(defn shape-add
  ([shape-a shape-b]
   (doto (Area. shape-a)
     (.add (Area. shape-b))))
  ([shape-a shape-b & more-shapes]
   (reduce shape-add (shape-add shape-a shape-b) more-shapes)))

(defn point
  ([c theta radius]
   (point c theta radius 1))
  ([[cx cy] theta radius ar]
   (let [x (+ cx (* radius (Math/sin theta)))
         y (+ cy (* radius ar (Math/cos theta)))]
     [x y])))

(defn poly
  ([x y r n angle] (poly x y r n angle 1))
  ([x y r n angle aspect-ratio]
   (let [angles (->> (range 0 TAU (/ TAU n))
                     (map (partial + angle)))
         points (map (fn [angle]
                       (point [x y] angle r aspect-ratio)
                       #_[(-> angle Math/sin (* r) (+ x))
                        (-> angle Math/cos (* r aspect-ratio) (+ y))])
                     angles)]
     (poly points)))
  ([xys]
   (let [xs (map first xys)
         ys (map second xys)]
     (Polygon. (int-array xs) (int-array ys) (count xys)))))

(defn bezier [a b c d n]
  (for [p (range 0 1.00001 (/ 1 n))]
    (let [e (vlerp a b p) f (vlerp b c p) g (vlerp c d p)
          h (vlerp e f p) i (vlerp f g p)
          j (vlerp h i p)]
      j)))

(def cubic bezier)

(defn heuristic-n [& points]
  ;(println points)
  ;(println (partition-all 2 1 points))
  (-> (reduce (fn [total [[x1 y1] [x2 y2]]]
                (+ total (abs (- x1 x2)) (abs (- y1 y2))))
              0
              (partition 2 1 points))
      (/ 2)
      (max 2)))

(defn quadratic
  ([a b c]
   (quadratic a b c (heuristic-n a b c)))
  ([a b c n]
   (for [p (range 0 1.00001 (/ 1 n))]
     (let [d (vlerp a b p) e (vlerp b c p)
           f (vlerp d e p)]
       f))))

(defn circle-path [c radius steps ar]
  (for [p (props/steps steps)]
    (point c (TAUS p) radius ar)))

(defn arc-points [[x y] r arc-start arc-end]
  (for [theta (range arc-start arc-end 0.05)]
    [(+ x (* r (Math/sin theta)))
     (+ y (* r (Math/cos theta)))]))

(defn heart-shape [[x y] size]
  (let [x-spread 0.45
        r (* size 1/2)
        r-bit (* r (Math/sqrt 0.5))
        x (int x)
        y (int y)
        x1 (- x (* size x-spread))
        x2 (+ x (* size x-spread))
        points-left (arc-points [(- x1 0.5) (- y 0.1)] r (* Math/PI 0.64) (* Math/PI 1.75))
        points-right (arc-points [(- x2 0.5) (- y 0.1)] r (* Math/PI 0.25) (* Math/PI 1.36))
        heart-shape (poly (concat points-right
                                  points-left
                                  [[x (+ y r-bit r-bit (* size x-spread))]]))]
    heart-shape))

(defn loop-path [path]
  (concat path [(first path)]))

(defn segments [path]
  (partition 2 1 path))

(defn walk-path [points f]
  (for [[idx [a b]] (map-indexed vector (partition 2 1 points))
        :let [p (/ idx (count points))]]
    (f a b p)))

(defn transforms [& tfs]
  (reduce (fn [a b]
            (.preConcatenate a b)
            a)
          (AffineTransform.)
          tfs))

(defn translate [shape x y]
  (doto (Area. shape)
    (.transform (AffineTransform/getTranslateInstance x y))))

(defn center [shape x y]
  (let [bounds (.getBounds shape)]
    (doto (Area. shape)
      (.transform (transforms
                    (AffineTransform/getTranslateInstance (- (.getX bounds)) (- (.getY bounds)))
                    (AffineTransform/getTranslateInstance (- (* (.getWidth bounds) 1/2)) (- (* (.getHeight bounds) 1/2)))
                    (AffineTransform/getTranslateInstance x y))))))

(defn rotate [shape radians]
  (doto (Area. shape)
    (.transform (AffineTransform/getRotateInstance radians))))

(defn scale [shape scale]
  (doto (Area. shape)
    (.transform (AffineTransform/getScaleInstance scale scale))))

(defn text->shape
  ([^Graphics2D g style ^String text] (text->shape g style text 0 0))
  ([^Graphics2D g style ^String text x y]
   (protocols/prepare-for-draw style g)
   (let [font (.getFont g)
         frc (.getFontRenderContext g)
         text-layout (TextLayout. text font frc)
         bounds (.getPixelBounds text-layout frc 0 0)
         gx (- x (.getX bounds) (/ (.getWidth bounds) 2)) ; TODO: use center?
         gy (- y (.getY bounds) (/ (.getHeight bounds) 2))]
     (.getOutline text-layout (AffineTransform/getTranslateInstance (float gx) (float gy))))))

(defn rectangle [x y w h]
  (Rectangle2D$Double. x y w h))

(defn ellipse
  ([[x y] r]
   (ellipse (- x r) (- y r) (* 2 r) (* 2 r)))
  ([x y r]
   (ellipse (- x r) (- y r) (* 2 r) (* 2 r)))
  ([x y w h]
   (Ellipse2D$Double. x y w h)))