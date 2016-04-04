(ns aoc.day6-fast
  (:require [clojure.string :as s]))

(def ^:const cmd-re #"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)")
(def ^:const grid-size 1000)

(defn make-grid
  "Construct the initial grid."
  []
  (loop [i (* grid-size grid-size)
         ret (transient [])]
    (if (pos? i)
      (recur (dec i) (conj! ret 0))
      (persistent! ret))))


(defn light [x y]
  (-> (dec grid-size)
      (* y) (+ x)))


(defn set-light!
  "Set the light at the given coord to v."
  [^longs grid ^long x ^long y ^long v]
  (assoc! grid (light x y) v))


(defn toggle-light!
  "Toggle the light at the given coord."
  [grid ^long x ^long y]
  (let [light (+ x (* y (dec grid-size)))]
    (if (zero? ^long (grid light))
      (assoc! grid light 1)
      (assoc! grid light 0))))


(defn parse-cmd
  "Given an input cmd, return a parsed cmd and coords."
  [input]
  (let [[_ cmd & coords] (re-matches cmd-re input)]
    (cons cmd (map #(Integer/parseInt %) coords))))


(defn process-cmd
  "Process a single command."
  [grid [cmd x1 y1 x2 y2]]
  (let [fun (case cmd
              "turn on"  (fn [x y] (set-light! grid x y 1))
              "turn off" (fn [x y] (set-light! grid x y 0))
              "toggle"   (fn [x y] (toggle-light! grid x y)))]
    (loop [grid grid
           ^long x x1
           ^long y y1]
      (if (<= ^long x ^long x2)
        (if (<= ^long y ^long y2)
          (recur (fun x y) x (inc y))
          (recur grid (inc x) y1))
        grid))))


(defn part-1
  "Execute all commands and then count number of lights that are ON."
  []
  (let [cmds (->> "resources/day6-part1-input"
                  slurp
                  s/split-lines
                  (map parse-cmd))
        grid (transient (make-grid))
        solved-grid (persistent! (reduce process-cmd grid cmds))]
    (count (filter pos? solved-grid))))


(defn process-cmd-revised
  "Process a single command according to revised instructions."
  [grid [cmd x1 y1 x2 y2]]
  (let [turn-on!  (fn [grid x y]
                    (set-light! grid x y
                                (inc (grid (light x y)))))
        turn-off! (fn [grid x y]
                    (set-light! grid x y
                                (max 0 (dec (grid (light x y))))))
        toggle!   (fn [grid x y]
                    (set-light! grid x y
                                (+ 2 (grid (light x y)))))
        fun (case cmd
              "turn on"  (fn [x y] (turn-on!  grid x y))
              "turn off" (fn [x y] (turn-off! grid x y))
              "toggle"   (fn [x y] (toggle!   grid x y)))]
    (loop [grid grid
           ^long x x1
           ^long y y1]
      (if (<= ^long x ^long x2)
        (if (<= ^long y ^long y2)
          (recur (fun x y) x (inc y))
          (recur grid (inc x) y1))
        grid))))


(defn part-2 []
  (let [cmds (->> "resources/day6-part1-input"
                  slurp
                  s/split-lines
                  (map parse-cmd))
        grid (transient (make-grid))
        solved-grid (persistent! (reduce process-cmd-revised grid cmds))]
    (reduce + solved-grid)))
