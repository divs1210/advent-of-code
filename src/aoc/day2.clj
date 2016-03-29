(ns aoc.day2
  (:require [clojure.string :as s]))

(defn part-1-input []
  (.trim (slurp "resources/day2-part1-input")))

(def part-2-input part-1-input)

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

(defn dims-from-input [input]
  (let [dim-strs (s/split input #"\n")]
    (for [dim-str dim-strs]
      (map #(Integer. %)
           (s/split dim-str #"x")))))

(defn part-1 []
  (reduce + (map paper-required-for-present
                 (dims-from-input (part-1-input)))))

(defn shortest-perimeter [dim]
  (let [[a b] (sort dim)]
    (* 2 (+ a b))))

(defn volume [[a b c]]
  (* a b c))

(defn ribbon-required-for-present [dim]
  (+ (shortest-perimeter dim)
     (volume dim)))

(defn part-2 []
  (reduce + (map ribbon-required-for-present
                 (dims-from-input (part-2-input)))))
