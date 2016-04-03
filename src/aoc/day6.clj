(ns aoc.day6
  (:require [clojure.set :as cset]
            [clojure.string :as s]))

(defn part-1-input []
  (.trim (slurp "resources/day6-part1-input")))

(defn str->point
  "(str->point \"0,0\") => [0 0]"
  [point-str]
  (map #(Integer. %)
       (s/split point-str #",")))

(defn str->rect
  "(str->rect \"toggle 0,0 through 2,2\" 7) => [[0 0] [2 2]]"
  [cmd-str start-index]
  (let [cmd-range (-> cmd-str
                      (.substring start-index)
                      .trim)
        [start-str _ end-str] (s/split cmd-range #" ")]
    (map str->point [start-str end-str])))

(defn str->cmd
  "(str->cmd \"toggle 0,0 through 2,2\") => [:toggle [[0 0] [2 2]]]"
  [s]
  (condp #(.startsWith %2 %1) s
    "turn on"  [:on (str->rect s 8)]
    "turn off" [:off (str->rect s 9)]
    "toggle"   [:toggle (str->rect s 7)]))

(defn cmds-from-input [in]
  (->> in
       s/split-lines
       (map str->cmd)))

(defn apply-cmd [lights-on [op [[x1 y1] [x2 y2]]]]
  (let [affected-lights (set (for [x (range x1 (inc x2))
                                   y (range y1 (inc y2))]
                               [x y]))]
    (case op
      :on  (cset/union lights-on affected-lights)
      :off (cset/difference lights-on affected-lights)
      :toggle (let [turn-off (cset/intersection lights-on affected-lights)
                    turn-on  (cset/difference affected-lights lights-on)]
                (-> lights-on
                    (cset/difference turn-off)
                    (cset/union turn-on))))))

(defn part-1 []
  (let [in (part-1-input)
        cmds (cmds-from-input in)]
    (count (reduce apply-cmd #{} cmds))))
