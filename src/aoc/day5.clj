(ns aoc.day5
  (:require [clojure.string :as s]))

(defn part-1-input []
  (.trim (slurp "resources/day5-part1-input")))

(def part-2-input part-1-input)

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
  (map #(.trim %)
       (s/split in #"\n")))

(defn part-1 []
  (->> (part-1-input)
       strs-from-input
       (filter nice?)
       count))

(defn has-at-least-one-repeated-letter-pair? [s]
  (let [all-pairs (partition 2 1 s)
        grouped-repeating-pairs (partition-by identity all-pairs)
        non-repeating-pairs (mapcat #(take-nth 2 %)
                                    grouped-repeating-pairs)
        occurences (vals (frequencies non-repeating-pairs))]
    ((comp not nil?) (some #(> % 1)
                           occurences))))

(defn has-at-least-one-repeated-letter-separated-by-1? [s]
  ((comp not nil?) (some (fn [[a _ c]]
                           (= a c))
                         (partition 3 1 s))))

(defn revised-nice? [s]
  (and (has-at-least-one-repeated-letter-pair? s)
       (has-at-least-one-repeated-letter-separated-by-1? s)))

(defn part-2 []
  (->> (part-2-input)
       strs-from-input
       (filter revised-nice?)
       count))
