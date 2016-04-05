(ns aoc.day6-another
  (:require [clojure.java.io :as io]))

(def ^:const cmd-re #"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)")
(def ^:const grid-size 1000)

(defn input
  "Raw input for the problem."
  []
  (-> "day6-part1-input"
      io/resource
      io/reader
      line-seq))


(defn make-grid
  "Construct the initial grid."
  []
  (loop [i (* grid-size grid-size)
         ret (transient [])]
    (if (pos? i)
      (recur (dec i) (conj! ret 0))
      (persistent! ret))))


(defn light-idx
  "Grid index for given coordinates."
  [x y]
  (-> (dec grid-size)
      (* y) (+ x)))


(defn parse-cmd
  "Given an input cmd, return a parsed cmd and coords."
  [input]
  (let [[_ cmd & coords] (re-matches cmd-re input)]
    (cons cmd (map #(Integer/parseInt %) coords))))


(defn instruction
  "Convert a command into an on/off/toggle instruction with
  indexes of all affected lights."
  [[cmd x1 y1 x2 y2]]
  (let [affected-light-idxs (for [x (range x1 (inc x2))
                                  y (range y1 (inc y2))]
                              (light-idx x y))]
    [cmd affected-light-idxs]))


(defn instructions-from-input
  "An ordered sequence of instructions from raw input"
  [in]
  (map (comp instruction parse-cmd) in))


(defn set-light!
  "Set the light at the given coord to v."
  [grid idx v]
  (assoc! grid idx v))


(defn process-instruction!
  "Executes instruction and updates grid"
  [apply-cmd-fn grid [cmd light-idxs]]
  (loop [grid grid
         idxs-left light-idxs]
    (if-not idxs-left
      grid
      (let [idx (first idxs-left)]
        (recur (apply-cmd-fn grid idx cmd)
               (next idxs-left))))))


(defn solve
  "Solve for given input using given apply-cmd-fn"
  [in apply-cmd-fn]
  (let [grid (make-grid)
        instructions (instructions-from-input in)]
    (loop [grid (transient grid)
           instructions-left instructions]
      (if-not instructions-left
        (reduce + (persistent! grid))
        (recur (process-instruction! apply-cmd-fn grid (first instructions-left))
               (next instructions-left))))))


;;;;;;;;;;;; Solution to Part 1 ;;;;;;;;;;;;;
(defn apply-cmd-part-1!
  [grid idx cmd]
  (set-light! grid idx (case cmd
                         "turn on"  1
                         "turn off" 0
                         "toggle"   (- 1 (grid idx)))))


(defn part-1
  "Solution to part-1"
  []
  (solve (input) apply-cmd-part-1!))


;;;;;;;;;;;; Solution to Part 2 ;;;;;;;;;;;;;
(defn apply-cmd-part-2!
  [grid idx cmd]
  (set-light! grid idx (case cmd
                         "turn on"  (inc (grid idx))
                         "turn off" (max 0 (dec (grid idx)))
                         "toggle"   (+ 2 (grid idx)))))


(defn part-2
  "Solution to part-2"
  []
  (solve (input) apply-cmd-part-2!))
