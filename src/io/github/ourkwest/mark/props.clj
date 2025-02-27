(ns io.github.ourkwest.mark.props
  (:require
    [io.github.ourkwest.mark.fundaments :refer :all]))

(defn steps-incl "Returns a series of n evenly spaced numbers between 0 and 1 inclusive." [n]
  (if (< n 2)
    '(0 1)
    (range 0 (inc (/ 1 (inc n))) (/ 1 (dec n)))))

(defn steps "Returns a series of n evenly spaced numbers from 0 inclusive up to and excluding 1." [n]
  (take n (steps-incl (inc n))))

(defn steps-excl "Returns a series of n evenly spaced numbers between 0 and 1 exclusive." [n]
  (if (< n 1)
    '()
    (range (/ 1 (inc n)) (/ n n) (/ 1 (inc n)))))

(defn p->sin
  "p is a proportion between 0 and 1, result is a proportion between 0 and 1
  0   -> 0
  1/2 -> 1
  1   -> 0"
  [p]
  (/ (- 1 (Math/cos (TAUS p))) 2))
