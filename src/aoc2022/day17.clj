(ns aoc2022.day17
  "https://adventofcode.com/2022/day/17"
  (:require
   [aoc2022.tools :as tools]))

;; Initial board and pos for rocks

(def R1 [[4 2] [4 3] [4 4] [4 5]])
(def R2 [[4 3] [5 2] [5 3] [5 4] [6 3]])
(def R3 [[4 2] [4 3] [4 4] [5 4] [6 4]])
(def R4 [[4 2] [5 2] [6 2] [7 2]])
(def R5 [[4 2] [4 3] [5 2] [5 3]])
(def ROCKS [R1 R2 R3 R4 R5])
(def BOARD [[0 1 2 3 4 5 6]])
(def PATTERN-TEST ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

;; Logic

(defn board-init
  [pattern]
  {:top-row 0
   :pattern pattern
   :pattern-idx 0
   :rock nil
   :rocks ROCKS
   :rock-idx 0
   :poss BOARD
   :max-col 6
   :hit? false})

(defn find-highest-row
  "Return highest row in the sequence of coords [[row1 col1] [row2 col2]...]"
  [coords]
  (apply max (map first coords)))

(defn spawn-rock
  "Update board with the spawned rock"
  [board iteration]
  (let [rocks (:rocks board)
        top-row (:top-row board)
        idx (mod iteration (count rocks))
        rock (nth rocks idx)]
    (assoc board :rock (mapv (fn [[row col]] [(+ top-row row) col]) rock))))

(defn insert-sorted
  "Return sorted vector of unique values with the given value inserted"
  [coll v]
  (into [] (-> coll
               set
               (conj v)
               sort)))

(defn land-rock
  "Update the board with the rock's coords"
  [board]
  (let [{:keys [poss rock top-row]} board
        rock-top-row (find-highest-row rock)
        top (max rock-top-row top-row)
        poss-new (reduce (fn [acc [row col]]
                           (let [vs (or (nth acc row nil) [])
                                 vs-new (insert-sorted vs col)]
                             (assoc acc row vs-new))) poss (sort rock))]
    (assoc board :top-row top :poss poss-new :hit? false)))

(defn in?
  "contains? for vectors"
  [coll el]
  (some #(= % el) coll))

(defn hit-walls?
  [board]
  (loop [rock-poss (:rock board)]
    (if (empty? rock-poss)
      false
      (let [[row col] (first rock-poss)]
        (if (or (< col 0)
                (> col (:max-col board))
                (< row 0))
          true
          (recur (next rock-poss)))))))

(defn hit-other-rocks?
  [board]
  (loop [rock-poss (:rock board)]
    (if (empty? rock-poss)
      false
      (let [[row col] (first rock-poss)
            row-vals (nth (:poss board) row nil)]
        (if (and row-vals (in? row-vals col))
          true
          (recur (next rock-poss)))))))

(defn hit?
  "Return true if the current rock hits the walls, bottom or other rocks"
  [board]
  (or (hit-walls? board) (hit-other-rocks? board)))

(defn direction
  "Return horizontal direction based on the current iteration"
  [board]
  (let [pattern (:pattern board)
        idx (mod (:pattern-idx board) (count pattern))]
    (nth pattern idx)))

(defmulti update-rock :to)

(defmethod update-rock :left
  [params]
  (mapv (fn [[row col]] [row (dec col)]) (:rock params)))

(defmethod update-rock :right
  [params]
  (mapv (fn [[row col]] [row (inc col)]) (:rock params)))

(defmethod update-rock :down
  [params]
  (mapv (fn [[row col]] [(dec row) col]) (:rock params)))

(defn move-horizontally
  "Update the board with the rock being pushed horizontally"
  [board]
  (let [rock-new (case (direction board)
                   \< (update-rock {:to :left :rock (:rock board)})
                   \> (update-rock {:to :right :rock (:rock board)}))
        board-old (update board :pattern-idx inc)
        board-new (assoc board-old :rock rock-new)]
    (if (hit? board-new)
      board-old
      (assoc board-new :rock rock-new))))

(defn move-down
  "Update the board with the rock fallen"
  [board]
  (let [rock-new (update-rock {:to :down :rock (:rock board)})
        board-new (assoc board :rock rock-new)]
    (if (hit? board-new)
      (assoc board :hit? true)
      board-new)))

(defn move-rock
  "Return resulting board once given rock is settled"
  [board]
  (if (:hit? board)
    (land-rock board)
    (let [board-new (-> board
                        (move-horizontally)
                        (move-down))]
      (recur board-new))))

(defn- process
  [board counter iterations]
  (if (= counter iterations)
    board
    (let [board-new (-> board
                        (spawn-rock counter)
                        (move-rock))]
      (recur board-new (inc counter) iterations))))

(defn process-rocks
  "Update board after processing given number of iterations"
  [board iterations]
  (process board 0 iterations))

;; launch

(defn get-board
  []
  (let [pattern (-> (tools/input-path)
                    tools/path->line)]
    (board-init pattern)))

(defn tower-height
  [board rocks]
  (let [board-processed (process-rocks board rocks)]
    (:top-row board-processed)))

;; testing
(defn part-test
  [rocks]
  (let [board (board-init PATTERN-TEST)]
    (:top-row (process-rocks board rocks))))

;; 3217 - 60ms
(defn part1 [] (tower-height (get-board) 2022))

;; FIXME improve performance ideas
;; Find cyclic patterns in the {:poss board}
;; The pattern of rocks must repeat itself
;; Once pattern is known, find out
;; how many patterns 1000000000000 includes.
;; To find a pattern, run more emulations,
;; e.g. (* (count (:pattern board)) 10) ~= 100k rocks
;; How to find a pattern?
;; 1. Every time (= (mod (count (rest (:poss board))) 2) 0),
;;    divide vector into two parts and compare, or
;; 2. Instead of vectors of values, compare vector of height growth/delta
;;    after each rock is landed
(defn part2 [] (tower-height (get-board) 1000000000000))
