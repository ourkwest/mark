(ns io.github.ourkwest.mark.fundaments
  (:import
    [java.lang Math$RandomNumberGeneratorHolder]
    [java.util Random Collections ArrayList Collection]))


(def TAU (* 2 Math/PI))
(defn TAUS [& numbers] (apply * TAU numbers))

(defn ->degrees [radians]
  (-> radians (/ TAU) (* 360)))

(defn ->radians [degrees]
  (-> degrees (/ 360) (* TAU)))

(defmacro forcat
  "Like `clojure.core/for` but concatenates the results."
  [seq-exprs body-expr]
  `(apply concat (for ~seq-exprs ~body-expr)))

(def millis-per-inch 25.4)

(defn clamp [lower x upper]
  (-> x
      (max lower)
      (min upper)))

(comment
  (defonce rng (-> Math$RandomNumberGeneratorHolder
                   (.getDeclaredField "randomNumberGenerator")
                   (doto (.setAccessible true))
                   (.get nil)))

  (defn set-random-seed [seed]
    (.setSeed ^Random rng seed))

  (defn shuffled [coll]
    (doto (ArrayList. ^Collection coll)
      (Collections/shuffle rng))))

(defn rand-0 [n]
  (- (rand n) (/ n 2)))

(defn v-extend [v n]
  (vec (take n (concat v (repeat n 0)))))

(defn v+
  ([v] v)
  ([v1 v2]
   (mapv + v1 v2))
  ([v1 v2 & more] ; (apply mapv + v1 v2 more) ???
   (reduce v+ (v+ v1 v2) more)))

(defn v- [v1 v2]
  (mapv - v1 v2))

(defn v* [v sf]
  (mapv #(* sf %) v))

(defn magnitude [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))

(defn normal "Returns a perpendicular vector" [[x y]]
  [(- y) x])

(defn normalize "Scales to magnitude 1" [[x y :as v]]
  (let [m (magnitude v)]
    [(/ x m) (/ y m)]))

(defn vlerp [v1 v2 p]
  (let [q (- 1 p)]
    (v+ (v* v1 q) (v* v2 p))))

(defn vwalk
  "walk a distance from v1 to v2"
  ([v1 v2 distance]
   (vwalk v1 v2 distance 1.0))
  ([v1 v2 distance prop-limit]
   (vlerp v1 v2 (min (/ distance (magnitude (v- v2 v1))) prop-limit))))

(defn v-rotate
  ([v angle]
   (v-rotate v [0 0] angle))
  ([v center angle]
   (let [[cx cy] center
         [x y z] v]
     [(+ cx
         (* (- x cx) (Math/cos angle))
         (* (- y cy) (Math/sin angle)))
      #_(+ cy
         (* (- y cy) (Math/cos angle))
         (* (- x cx) (- (Math/sin angle))))
      (+ cy
         (* (- y cy) (- (Math/cos angle)))
         (* (- x cx) (Math/sin angle)))
      (or z 0)])))

(defn lerp [a b p]
  (let [q (- 1 p)]
    (+ (* a q) (* b p))))

(defn intersect-0 "returns the intersection point of the lines [p0 p1] and [p2 p3]"
  [[[p0-x p0-y] [p1-x p1-y]] [[p2-x p2-y] [p3-x p3-y]]]
  (let [s1-x (- p1-x p0-x)
        s1-y (- p1-y p0-y)
        s2-x (- p3-x p2-x)
        s2-y (- p3-y p2-y)
        det (+ (* (- s2-x) s1-y)
               (* s1-x s2-y))]
    (when-not (zero? det)
      (let [s (/ (+ (* (- s1-y) (- p0-x p2-x))
                    (* s1-x (- p0-y p2-y)))
                 det)
            t (/ (- (* (- s2-x) (- p0-y p2-y))
                    (* s2-y (- p0-x p2-x)))
                 det)]
        ;(println s1-x s1-y s2-x s2-y s t)
        (when (and (>= s 0) (<= s 1)
                   (>= t 0) (<= t 1))
          [(+ p0-x (* t s1-x))
           (+ p0-y (* t s1-y))])))))

(defn intersect [line-a line-b]
  (or (intersect-0 line-a line-b)
      (intersect-0 (reverse line-a) line-b)))
