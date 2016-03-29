(ns aoc.day3)

(defn part-1-input []
  (.trim (slurp "resources/day3-part1-input")))

(def part-2-input part-1-input)

(defn move [pos dir]
  (mapv + pos (case dir
                \^ [ 0 -1]
                \v [ 0  1]
                \> [ 1  0]
                \< [-1  0])))

(defn poses-visited [path]
  (loop [curr-pos [0 0]
         visited #{}
         path-left path]
    (let [visited (conj visited curr-pos)]
      (if-not path-left
        visited
        (recur (move curr-pos (first path-left))
               visited
               (next path-left))))))

(defn part-1 []
  (count (poses-visited (part-1-input))))

(defn part-2 []
  (let [input-path (part-2-input)
        santa-path (take-nth 2 input-path)
        robo-santa-path (take-nth 2 (rest input-path))

        visited-by-santa (poses-visited santa-path)
        visited-by-robo-santa (poses-visited robo-santa-path)]
    (count (clojure.set/union visited-by-santa visited-by-robo-santa))))
