(ns io.github.ourkwest.mark.color
  (:require
    [clojure.string :as str]
    [io.github.ourkwest.mark.fundaments :refer :all]
    [clojure.string :as string]
    [io.github.ourkwest.mark.props :as props])
  (:import
    [clojure.lang IPersistentVector]
    [java.awt Color]))


(defprotocol Chromatic
  (bits [chromatic] "Returns [r g b a]")
  (string [chromatic] "Returns 'rgba(r,g,b,a)'")
  (awt-color [chromatic] "Returns a java.awt.Color")
  (integer [chromatic] "Returns an integer"))

(defn rgb
  ([color-bits]
   (if (satisfies? Chromatic color-bits)
     (apply rgb (bits color-bits))
     (apply rgb color-bits)))
  ([r g b]
   (Color. (int (clamp 0 r 255))
           (int (clamp 0 g 255))
           (int (clamp 0 b 255))))
  ([r g b a]
   (Color. (int (clamp 0 r 255))
           (int (clamp 0 g 255))
           (int (clamp 0 b 255))
           (int (clamp 0 a 255)))))

(extend-protocol Chromatic

  Color
  (bits [color] [(.getRed ^Color color) (.getGreen ^Color color) (.getBlue ^Color color) (.getAlpha ^Color color)])
  (string [color] (string (bits color)))
  (awt-color [color] color)
  (integer [color] (.getRGB color))

  String
  (bits [color-string]
    (cond
      (string/starts-with? color-string "#")
      (bits (-> color-string (subs 1) (Integer/parseInt 16) (Color.)))

      (string/starts-with? color-string "rgb")
      (take 4 (concat (mapv parse-long (re-seq #"\d+" color-string)) [0 0 0 0]))))
  (string [color-string] color-string)
  (awt-color [color-string] (awt-color (bits color-string)))

  IPersistentVector
  (bits [vector-of-bits] vector-of-bits)
  (string [vector-of-bits] (str "rgba(" (str/join "," vector-of-bits) ")"))
  (awt-color [[r g b a]] (Color. (int r) (int g) (int b) (int a)))
  (integer [vector-of-bits] (integer (awt-color vector-of-bits))))

(defn to-awt-color [x]
  (cond
    (satisfies? Chromatic x) (awt-color x)
    (integer? x) (Color. x)))

(defn with-alpha [color alpha]
  (let [[r g b] (bits color)]
    (rgb r g b alpha)))

(defn with-light [color brightness]
  ; TODO: handle edge cases ? i.e. (with-light Color/GREEN 200) doesn't produce an average brightness of 200 across all three channels
  (let [[r g b a] (bits color)
        f (/ (* 3 brightness) (+ r g b))]
    (rgb (* f r) (* f g) (* f b) a)))

(defn rgb-lerp [c1 c2 p]
  (apply rgb (vlerp (bits c1) (bits c2) p)))

(defn grayify [color]
  (let [[r g b a] (bits color)
        light (int (/ (+ r g b) 3))]
    (Color. light light light (int a))))

(defn to-rgba-str [chromatic]
  (let [[r g b a] (bits chromatic)]
    (str "rgba("r","g","b","a")")))

(defn to-rgb-str [chromatic]
  (let [[r g b _] (bits chromatic)]
    (str "rgb("r","g","b")")))

(defn color-wheel
  "Returns a rainbow of high saturation, high brightness colors in hue order.
  n - number of colors
  offset - number between 0 and 1, how far around the wheel to shift the colors"
  ([n] (color-wheel n 0))
  ([n offset]
   (for [hue (map (partial + offset) (props/steps n))]
     (Color/getHSBColor hue 1.0 1.0))))

(comment

  (require '[mark.props])
  (def file "/home/rachel/orgs/archwaytheatre.github.io/docs/css/style.css")
  (defn spread [colors prefix name-1 name-2]
    (doseq [[i p] (map-indexed vector (mark.props/steps 5))]
      (println (str "    --arch-" prefix "-" (inc i) ": "
                    (to-rgb-str (rgb-lerp (colors name-1) (colors name-2) p))
                    ";"))))

  (let [colors (->> file slurp string/split-lines string/join
                    (re-seq #"    --(arch-[a-z]+-[0-9]): (rgb\([0-9, ]+\));")
                    (map rest)
                    (map vec)
                    (into {}))]
    (spread colors "main" "arch-pink-1" "arch-pink-2")
    (spread colors "studio" "arch-blue-1" "arch-blue-2")
    (spread colors "online" "arch-pear-1" "arch-pear-2")
    (spread colors "unknown" "arch-grey-1" "arch-grey-2")
    )

  )
