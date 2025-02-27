(ns io.github.ourkwest.mark.shape
  (:require
    [io.github.ourkwest.mark.fundaments :refer :all]
    [io.github.ourkwest.mark.props :as props]
    [io.github.ourkwest.mark.protocols :as protocols])
  (:import
    [java.awt Graphics2D Polygon Shape]
    [java.awt.font TextLayout]
    [java.awt.geom AffineTransform Area Ellipse2D$Double PathIterator Rectangle2D$Double]))



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

(defn arc-point
  ([c theta radius]
   (arc-point c theta radius 1))
  ([[cx cy] theta x-radius y-radius]
   [(+ cx (* x-radius (Math/sin theta)))
    (- cy (* y-radius (Math/cos theta)))]))

(defn poly
  ([c n radius angle]
   (poly c n radius radius angle))
  ([c n x-radius y-radius angle]
   (poly (->> (range 0 TAU (/ TAU n))
              (map (partial + angle))
              (map #(arc-point c % x-radius y-radius)))))
  ([xys]
   (let [xs (map first xys)
         ys (map second xys)]
     (Polygon. (int-array xs) (int-array ys) (count xys)))))

(defn heuristic-n [& points]
  (-> (reduce (fn [total [[x1 y1] [x2 y2]]]
                (+ total (abs (- x1 x2)) (abs (- y1 y2))))
              0
              (partition 2 1 points))
      (/ 2)
      (max 2)))

(defn bezier
  ([a b c d]
   (bezier a b c d (heuristic-n a b c d)))
  ([a b c d n]
   (for [p (range 0 1.00001 (/ 1 n))]
     (let [e (vlerp a b p) f (vlerp b c p) g (vlerp c d p)
           h (vlerp e f p) i (vlerp f g p)
           j (vlerp h i p)]
       j))))

(def cubic bezier)

#_(defn smooth-bezier-path
  "Make a path from pairs of points.
  The first point in each pair is a vector (relative position) from the last position (initially [0,0]).
  The second point in each pair is the control vector (relative to the point).
  For each pair of pairs a bezier is created, the resulting beziers are concatenated."
  [n points]
  (loop [[p1 c1 p2 c2 :as points] points
         location [0 0]
         path [(first points)]]
    (if p2
      (let [p1' (v+ location p1)
            p2' (v+ p1' p2)
            c1' (v+ p1' c1)
            c2' (v- p2' c2)]
        (recur (drop 2 points)
               p1'
               (concat path (rest (bezier p1' c1' c2' p2' n)))))
      path))

  #_(cons (first points)
        (forcat [[p1 v1 p2 v2] (partition 4 2 points)]
          (let [p2 (v+ p1 p2)
                v2 (v- p2 v2)]
            (rest (bezier p1 v1 v2 p2 n))))))

(defn smooth-bezier-path
  "Make a path from pairs of points.
  For each pair of pairs a bezier is created, the resulting beziers are concatenated.

  points should be a sequence of alternating points and control points, extra control points are hallucinated by
  inverting each control point about its preceeding point."
  [n points]
  (cons (first points)
        (forcat [[p1 c1 p2 c2] (partition 4 2 points)]
          (let [c2' (v- p2 (v- c2 p2))]
            (rest (bezier p1 c1 c2' p2 n))))))

(defn quadratic
  ([a b c]
   (quadratic a b c (heuristic-n a b c)))
  ([a b c n]
   (for [p (range 0 1.00001 (/ 1 n))]
     (let [d (vlerp a b p) e (vlerp b c p)
           f (vlerp d e p)]
       f))))

(defn relativize [& points]
  (loop [[p & points] points
         location [0 0]
         accumulator []]
      (if p
        (let [loc' (v+ location p)]
          (recur (seq points) loc' (conj accumulator loc')))
        accumulator)))

(defn circle-path [c radius steps ar]
  (for [p (props/steps steps)]
    (arc-point c (TAUS p) radius ar)))

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

(defn loop-path
  ([path]
   (loop-path path 1))
  ([path overlap]
   (concat path (take overlap path))))

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

(defn get-center [shape]
  (let [bounds (.getBounds shape)]
    [(+ (.getX bounds) (* (.getWidth bounds) 1/2))
     (+ (.getY bounds) (* (.getHeight bounds) 1/2))]))

(defn rotate [shape radians]
  (doto (Area. shape)
    (.transform (AffineTransform/getRotateInstance radians))))

(defn rotate-in-place [shape radians]
  (let [[cx cy] (get-center shape)]
    (-> shape
        (rotate radians)
        (center cx cy))))

(defn scale [shape scale]
  (doto (Area. shape)
    (.transform (AffineTransform/getScaleInstance scale scale))))

(defn scale-in-place [shape scale-factor]
  (let [[cx cy] (get-center shape)]
    (-> shape
        (scale scale-factor)
        (center cx cy))))

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

(defn rectangle
  ([[x1 y1] [x2 y2]]
   (rectangle x1 y1 (- x2 x1) (- y2 y1)))
  ([x y w h]
   (Rectangle2D$Double. x y w h)))

(defn ellipse
  ([{:keys [x y p r xr yr w h]}]
   (let [[x y] (or p [x y])]
     (cond
       (and x y r) (ellipse (- x r) (- y r) (* 2 r) (* 2 r))
       (and x y w h) (ellipse x y w h)
       (and x y xr yr) (ellipse (- x xr) (- y yr) (* 2 xr) (* 2 yr))
       :else (throw (IllegalArgumentException. "Missing arg.")))))
  ([[x y] r] ; deprecated
   (ellipse (- x r) (- y r) (* 2 r) (* 2 r)))
  ([x y r] ; deprecated
   (ellipse (- x r) (- y r) (* 2 r) (* 2 r)))
  ([x y w h]
   (Ellipse2D$Double. x y w h)))

(defn path-segments [shape]
  (let [pi (.getPathIterator ^Shape shape nil)
        array ^doubles (double-array 6)
        p1 #(vector (aget array 0) (aget array 1))
        p2 #(vector (aget array 2) (aget array 3))
        p3 #(vector (aget array 4) (aget array 5))]
    (loop [segments-so-far []
           current-segment []]
      (if (.isDone ^PathIterator pi)
        (conj segments-so-far current-segment)
        (let [seg-type (.currentSegment pi array)]
          (.next pi)
          (case seg-type
            0 (recur segments-so-far [(p1)])
            1 (recur segments-so-far (conj current-segment (p1)))
            2 (recur segments-so-far (concat current-segment (quadratic (last current-segment) (p1) (p2))))
            3 (recur segments-so-far (concat current-segment (cubic (last current-segment) (p1) (p2) (p3))))
            4 (recur (conj segments-so-far current-segment) [])))))))

(defn pixels
  ([[x1 y1] [x2 y2]]
   (let [x-diff (- x2 x1)
         y-diff (- y2 y1)
         steps (inc (max (abs x-diff) (abs y-diff)))]
     (for [n (props/steps steps)]
       [(int (Math/round (double (+ x1 (* n x-diff)))))
        (int (Math/round (double (+ y1 (* n y-diff)))))])))
  ([path]
   (forcat [[a b] (partition 2 1 path)]
     (pixels a b))))
