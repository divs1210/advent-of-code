(ns aoc.day2
  (:require [clojure.string :as s]))

(defn part-1-input []
  (.trim (slurp "resources/day2-part1-input")))

(defn surface-area [[a b c]]
  (* 2 (+ (* a b)
          (* b c)
          (* a c))))

(defn slack [[a b c]]
  (min (* a b)
       (* b c)
       (* a c)))

(defn paper-required-for-present [dim]
  (+ (surface-area dim)
     (slack dim)))

(defn part-1 []
  (let [dim-strs (s/split (part-1-input) #"\n")
        dims (for [dim-str dim-strs]
               (map #(Integer. %)
                    (s/split dim-str #"x")))]
    (reduce + (map paper-required-for-present dims))))
