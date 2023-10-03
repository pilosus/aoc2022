(ns aoc2022.day10
  "https://adventofcode.com/2022/day/10"
  (:require
   [aoc2022.aoc2022 :as tools]
   [clojure.string :as string]))

(defn line->cmd
  [l]
  (let [[cmd v] (string/split l #"\s+")
        command (keyword cmd)
        value (if (= command :addx) (Integer/parseInt v) 0)
        cycles (if (= command :addx) 2 1)]
    [command value cycles]))

(defn generate-register-states
  "Return a vector of register state at any given cycle. Cycle number
  equals to theq vector index. Cycle numbers start from 1"
  [init command-seq]
  (loop [commands command-seq
         register init
         states [init]]
    (if (empty? commands)
      (conj states register)
      (let [[_ v cycles] (first commands)
            new-register (+ register v)
            new-states (->> register
                            (repeat cycles)
                            (into states))]
        (recur (next commands) new-register new-states)))))

(defn signal-strength
  "Return signal strength for a vector or register states"
  [register-states]
  (reduce
   (fn [acc idx]
     (let [v (nth register-states idx)
           strength (* v idx)]
       (+ acc strength)))
   0
   (range 20 (count register-states) 40)))

(defn calc-total-strength
  "Return total signal strength for the given list of commands"
  []
  (let [lines (-> (tools/input-path)
                  tools/path->lines)
        commands (->> lines
                      (map line->cmd))
        total-strength (->> commands
                            (generate-register-states 1)
                            signal-strength)]
    total-strength))

(comment
  ;; Part 1 - 13720
  (calc-total-strength)
  ;; Part 2 -
  )
