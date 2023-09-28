(ns aoc2022.day2
  "https://adventofcode.com/2022/day/2"
  (:require [clojure.string :as string]))

(def INPUT-PATH "resources/input/day2.txt")

(def letter->shape
  {"A" :rock
   "B" :paper
   "C" :scissors
   "X" :rock
   "Y" :paper
   "Z" :scissors})

(def letter->outcome
  {"X" :lost
   "Y" :draw
   "Z" :won})

(def weights
  {:rock 1
   :paper 2
   :scissors 3})

(def outcomes
  {:won 6
   :draw 3
   :lost 0})

(defn find-second-shape
  "Given a shape for the first player and the outcome, return the shape
  for the second player"
  [[shape-1 outcome]]
  (cond
    (= outcome :draw) shape-1
    (and (= shape-1 :rock)
         (= outcome :won)) :paper
    (and (= shape-1 :rock)
         (= outcome :lost)) :scissors
    (and (= shape-1 :paper)
         (= outcome :won)) :scissors
    (and (= shape-1 :paper)
         (= outcome :lost)) :rock
    (and (= shape-1 :scissors)
         (= outcome :won)) :rock
    (and (= shape-1 :scissors)
         (= outcome :lost)) :paper))

(defn play-two-shapes
  "Return one of the outcomes for the player 2: :won, :draw, :lost"
  [[player-1 player-2]]
  (cond
    (and (= player-2 :rock)
         (= player-1 :paper)) :lost
    (and (= player-2 :rock)
         (= player-1 :scissors)) :won

    (and (= player-2 :paper)
         (= player-1 :rock)) :won
    (and (= player-2 :paper)
         (= player-1 :scissors)) :lost

    (and (= player-2 :scissors)
         (= player-1 :rock)) :lost
    (and (= player-2 :scissors)
         (= player-1 :paper)) :won
    :else :draw))

(defn game-total
  "Return total score for a single game for the player 2"
  [[player-1 player-2]]
  (let [outcome (play-two-shapes [player-1 player-2])
        game-score (get outcomes outcome)
        shape-score (get weights player-2)]
    (+ game-score shape-score)))

(defn path->lines
  [path]
  (-> path
      slurp
      string/split-lines))

(defn total-score-two-shapes
  "Return a total score for all games from the file where each game is a
  pair of shape-shape"
  [path]
  (let [lines (path->lines path)
        pairs (->> lines
                   (map #(string/split % #"\s+"))
                   (map (fn [v]
                          (let [[player-1 player-2] v]
                            [(get letter->shape player-1)
                             (get letter->shape player-2)]))))
        score (->> pairs
                   (map game-total)
                   (reduce + 0))]
    score))

(defn total-score-shape-outcome
  "Return a total score for all games from the file where each game is a
  pair of shape-outcome"
  [path]
  (let [lines (path->lines path)
        pairs (->> lines
                   (map #(string/split % #"\s+"))
                   (map (fn [v]
                          (let [[player-1 outcome-str] v
                                shape-1 (get letter->shape player-1)
                                outcome (get letter->outcome outcome-str)
                                shape-2 (find-second-shape [shape-1 outcome])]
                            [shape-1 shape-2]))))
        score (->> pairs
                   (map game-total)
                   (reduce + 0))]
    score))

(comment
  ;; Part 1
  (total-score-two-shapes INPUT-PATH)
  ;; Part 2
  (total-score-shape-outcome INPUT-PATH))
