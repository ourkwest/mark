(ns mark.fundaments
  (:import
    [java.lang Math$RandomNumberGeneratorHolder]
    [java.util Random Collections ArrayList Collection]))


(def TAU (* 2 Math/PI))
(defn TAUS [n] (* TAU n))

(defmacro forcat
  "Like `clojure.core/for` but concatenates the results."
  [seq-exprs body-expr]
  `(apply concat (for ~seq-exprs ~body-expr)))

(def millis-per-inch 25.4)

(defn clamp [lower x upper]
  (-> x
      (max lower)
      (min upper)))

(defonce rng (-> Math$RandomNumberGeneratorHolder
                 (.getDeclaredField "randomNumberGenerator")
                 (doto (.setAccessible true))
                 (.get nil)))

(defn set-random-seed [seed]
  (.setSeed ^Random rng seed))

(defn shuffled [coll]
  (doto (ArrayList. ^Collection coll)
    (Collections/shuffle rng)))

(defn rand-0 [n]
  (- (rand n) (/ n 2)))

(defn v+
  ([v] v)
  ([v1 v2]
   (mapv + v1 v2))
  ([v1 v2 & more]
   (reduce v+ (v+ v1 v2) more)))

(defn v- [v1 v2]
  (mapv - v1 v2))

(defn v* [v f]
  (mapv #(* f %) v))

(defn normal [[x y]]
  [(- y) x])

(defn magnitude [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))

(defn vlerp [v1 v2 p]
  (let [q (- 1 p)]
    (v+ (v* v1 q) (v* v2 p))))

(defn lerp [a b p]
  (let [q (- 1 p)]
    (+ (* a q) (* b p))))