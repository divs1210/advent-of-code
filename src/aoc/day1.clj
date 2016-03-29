(ns aoc.day1)

(defn part-1-input []
  (.trim (slurp "resources/day1-part1-input")))

(def part-2-input part-1-input)

(defn part-1 []
  (let [path (part-1-input)
        instructions (frequencies path)
        ups   (instructions \()
        downs (instructions \))]
    (- ups downs)))

(defn part-2 []
  (let [path (part-2-input)
        dirs (map #(case %
                     \(  1
                     \) -1
                     0)
                  path)
        steps (reductions + 0 dirs)]
    (.indexOf steps -1)))
