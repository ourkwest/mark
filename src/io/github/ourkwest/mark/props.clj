(ns mark.props
  (:require
    [mark.fundaments :refer :all]))



(defn steps "Returns a series of n evenly spaced numbers between 0 and 1 inclusive." [n]
  (range 0 (+ 1 (/ 1 (inc n))) (/ 1 (dec n))))

(defn p->sin
  "p is a proportion between 0 and 1, result is a proportion between 0 and 1
  0   -> 0
  1/2 -> 1
  1   -> 0"
  [p]
  (/ (- 1 (Math/cos (TAUS p))) 2))
