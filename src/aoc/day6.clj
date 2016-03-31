(ns aoc.day6
  (:require [clojure.core.reducers :as r]
            [clojure.set :as cset]
            [clojure.string :as s]))

(defn part-1-input []
  (.trim (slurp "resources/day6-part1-input")))

(defn str->rect [cmd-str start-index]
  (let [[start-str _ end-str] (s/split (-> cmd-str
                                           (.substring start-index)
                                           .trim)
                                       #" ")
        start (map #(Integer. %)
                   (s/split start-str #","))
        end   (map #(Integer. %)
                   (s/split end-str #","))]
    [start end]))

(defn str->cmd [s]
  (condp #(.startsWith %2 %1) s
    "turn on"  [:on (str->rect s 8)]
    "turn off" [:off (str->rect s 9)]
    "toggle"   [:toggle (str->rect s 7)]))

(defn cmds-from-input [in]
  (->> in
       s/split-lines
       (map str->cmd)))

(defn apply-cmd [cmd lights-on]
  (let [[op [[x1 y1] [x2 y2]]] cmd
        affected-lights (set (for [x (range x1 (inc x2))
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
    (loop [cmds-left cmds
           lights-on #{}]
      (if-not cmds-left
        (count lights-on)
        (recur (next cmds-left)
               (apply-cmd (first cmds-left) lights-on))))))

(defn part-1-par []
  (let [in (part-1-input)
        cmds (cmds-from-input in)]
    (count
     (r/fold (fn
               ([]
                #{})
               ([lights cmd]
                (apply-cmd cmd lights)))
             (cmds-from-input (part-1-input))))))
