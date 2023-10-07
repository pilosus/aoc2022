(ns aoc2022.day12
  "https://adventofcode.com/2022/day/11"
  (:require
   [aoc2022.tools :as tools]))

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
  [pos area]
  (let [{:keys [signals max-row max-col]} area
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
  hops from the start to a given position. If (get distance pos) is
  nil, that means the position is unreachable.

  Paths map has positions as keys and parent's position as
  value. Position map allows to track down the shortest path from a
  given position back to the start position.

  Algorithm is based on the breadth-first search algorithm. If hops
  could have had distance more than 1, Dijkstra's algorithm could have
  been used."
  [area]
  (let [{:keys [start finish]} area]
    (loop [queue [start]
           paths {start nil}
           distances {start 0}]
      (if (empty? queue)
        [distances paths]
        (let [curr (first queue)]
          (if (= curr finish)
            [distances paths]
            (let [[queue' paths' distances']
                  (loop [neighbours (pos->neighbours curr area)
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
  "Return the shortest path (number of hops) from start to finish"
  [area]
  (let [[distances _] (bfs area)]
    (get distances (:finish area))))

(defn find-shortest-path
  "Find the shortest path (number of hops) from start to finish"
  []
  (let [area (-> (tools/input-path)
                 tools/path->lines
                 init)]
    (shortest-path area)))

(defn find-starts
  "Return a vector of starting positions for the given area.
  Any position with value 0 (equivallent of `a`) can be a staring
  position."
  [area]
  (reduce
   (fn [init [k v]]
     (if (= v 0)
       (conj init k)
       init))
   []
   (:signals area)))

(defn shortest-path-multiple-starts
  "Find the shortest among the shortest paths starting from any possible
  starting position"
  []
  (let [area (-> (tools/input-path)
                 tools/path->lines
                 init)
        result (->> area
                    find-starts
                    (reduce
                     (fn [init s]
                       (let [area' (assoc area :start s)
                             shortest (shortest-path area')]
                         (conj init shortest)))
                     [])
                    ;; remove unreachable starting positions
                    (remove nil?)
                    sort
                    first)]
    result))

(comment
  ;; Part 1 - 391 - 30 ms
  (find-shortest-path)
  ;; Part 2 - 386 - 3000 ms
  (shortest-path-multiple-starts))
