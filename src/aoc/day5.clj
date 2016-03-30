(ns aoc.day5
  (:require [clojure.string :as s]))

(defn part-1-input []
  (.trim (slurp "resources/day5-part1-input")))

(defn has-at-least-three-vowels? [s]
  (let [letters (frequencies s)]
    (>= (+ (get letters \a 0)
           (get letters \e 0)
           (get letters \i 0)
           (get letters \o 0)
           (get letters \u 0))
        3)))

(defn has-at-least-one-repeated-letter? [s]
  ((comp not nil?) (some #(> (count %) 1)
                         (partition-by identity s))))

(defn has-no-taboos? [s]
  (not (or (.contains s "ab")
           (.contains s "cd")
           (.contains s "pq")
           (.contains s "xy"))))

(defn nice? [s]
  (and (has-at-least-three-vowels? s)
       (has-at-least-one-repeated-letter? s)
       (has-no-taboos? s)))

(defn strs-from-input [in]
  (s/split in #"\n"))

(defn part-1 []
  (->> (part-1-input)
       strs-from-input
       (filter nice?)
       count))
