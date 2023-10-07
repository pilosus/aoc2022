(ns aoc2022.day12
  "https://adventofcode.com/2022/day/11"
  (:require
   [aoc2022.tools :as tools]
   [clojure.string :as string]))

"
--- Day 12: Hill Climbing Algorithm ---

You try contacting the Elves using your handheld device, but the river
you're following must be too low to get a decent signal.

You ask the device for a heightmap of the surrounding area (your
puzzle input). The heightmap shows the local area from above broken
into a grid; the elevation of each square of the grid is given by a
single lowercase letter, where a is the lowest elevation, b is the
next-lowest, and so on up to the highest elevation, z.

Also included on the heightmap are marks for your current position (S)
and the location that should get the best signal (E). Your current
position (S) has elevation a, and the location that should get the
best signal (E) has elevation z.

You'd like to reach E, but to save energy, you should do it in as few
steps as possible. During each step, you can move exactly one square
up, down, left, or right. To avoid needing to get out your climbing
gear, the elevation of the destination square can be at most one
higher than the elevation of your current square; that is, if your
current elevation is m, you could step to elevation n, but not to
elevation o. (This also means that the elevation of the destination
square can be much lower than the elevation of your current square.)

For example:

Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi

Here, you start in the top-left corner; your goal is near the
middle. You could start by moving down or right, but eventually you'll
need to head toward the e at the bottom. From there, you can spiral
around to the goal:

v..v<<<<
>v.vv<<^
.>vv>E^^
..v>>>^^
..>>>>>^

In the above diagram, the symbols indicate whether the path exits each
square moving up (^), down (v), left (<), or right (>). The location
that should get the best signal is still E, and . marks unvisited
squares.

This path reaches the goal in 31 steps, the fewest possible.

What is the fewest steps required to move from your current position
to the location that should get the best signal?

To begin, get your puzzle input.
"

;; - parse input to a vector of vectors:
;;     - substitute letters with corresponing numbers
;;     - save start/finish positions
;; - breadth-first search
;;   - fn to find neigboughs for a given node (position):
;;     any adjacent cell in u/d/l/r direction that is (<= (- next curr) 1)
;; - shortest path -> Dijkstra algo?
;; https://stackoverflow.com/questions/8379785/how-does-a-breadth-first-search-work-when-looking-for-shortest-path

(defn find-key-by-val
  "Return a key mapping to the first occurrence of given value in the map"
  [m v]
  (let [[k _] (->> m (filter (fn [[k _]] (= (get m k) v)))
                   first)]
    k))

(defn init
  "Return a pos->height map, start and finish positions"
  [lines]
  (let [letter->idx (tools/letter->idx)
        matrix (->> lines
                    (map tools/str->vec)
                    tools/matrix-indexed)
        pos->height (reduce (fn [init l] (into init l)) {} matrix)
        start (find-key-by-val pos->height "S")
        finish (find-key-by-val pos->height "E")
        result (reduce
                (fn [init [k v]]
                  (let [v' (cond
                             (contains? letter->idx v) (get letter->idx v)
                             (= v "S") (get letter->idx "a")
                             (= v "E") (get letter->idx "z"))]
                    (assoc init k v')))
                {}
                pos->height)]
    {:signals result
     :start start
     :finish finish
     :max-row (dec (count matrix))
     :max-col (dec (count (first matrix)))}))

(defn neighbour-indicies
  [idx max-idx]
  (range (max 0 (dec idx)) (min max-idx (+ idx 2))))

(defn height-ok?
  "Return true if next height is at most 1 higher than the current one or lower"
  [curr-pos next-pos m]
  (let [curr-h (get m curr-pos)
        next-h (get m next-pos)]
    (<= (- next-h curr-h) 1)))

(defn potential-neigbours
  [[r-idx c-idx] max-row max-col]
  (let [potential [[r-idx (dec c-idx)]
                   [r-idx (inc c-idx)]
                   [(dec r-idx) c-idx]
                   [(inc r-idx) c-idx]]]
    (remove
     (fn [[r c]]
       (or (and (= r r-idx)
                (= c c-idx))
           (< r 0)
           (> r max-row)
           (< c 0)
           (> c max-col)))
     potential)))

(defn pos->neighbours
  "Return a vector of neighbours of the given position in the map"
  [pos game]
  (let [{:keys [signals max-row max-col]} game
        positions (potential-neigbours pos max-row max-col)
        neighbours (->> positions
                        (filter
                         (fn [neighbour-pos]
                           (height-ok? pos neighbour-pos signals)))
                        (into []))]
    neighbours))

(defn bfs
  "Return a distance map and paths map.
  Distance map has positions as keys and distances to from the start
  position as a value. Distance map allows to find minimum number of
  hops from the start to a given position.

  Paths map has positions as keys and parent's position as
  value. Position map allows to track down the shortest path from a
  given position back to the start position.

  Algorithm is based on the breadth-first search algorithm. If hops
  could have had distance more than 1, Dijkstra's algorithm could have
  been used."
  [game]
  (let [{:keys [start finish]} game]
    (loop [queue [start]
           paths {start nil}
           distances {start 0}]
      (if (empty? queue)
        [distances paths]
        (let [curr (first queue)]
          (if (= curr finish)
            [distances paths]
            (let [[queue' paths' distances']
                  (loop [neighbours (pos->neighbours curr game)
                         qi queue
                         pi paths
                         di distances]
                    (if (empty? neighbours)
                      [qi pi di]
                      (let [neighbour (first neighbours)]
                        ;; Inner loop recur
                        (if (contains? pi neighbour)
                          (recur (next neighbours) qi pi di)
                          (recur
                           (next neighbours)
                           (conj qi neighbour)
                           (assoc pi neighbour curr)
                           (assoc di neighbour (inc (get di curr))))))))]
              ;; Outer loop recur
              (recur
               (into [] (next queue'))
               paths'
               distances'))))))))

(defn shortest-path
  "Find the shortest path (number of hops) from start to finish"
  []
  (let [game (-> (tools/input-path)
                 tools/path->lines
                 init)
        [distances _] (bfs game)]
    (get distances (:finish game))))

(comment
  ;; Part 1 - 391
  (shortest-path))
