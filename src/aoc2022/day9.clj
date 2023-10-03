(ns aoc2022.day9
  "https://adventofcode.com/2022/day/9"
  (:require
   [aoc2022.aoc2022 :as tools]
   [clojure.string :as string]))

(defn head-path
  "Return a sequence of positions of the head given current position and a move"
  [[row-idx col-idx] [direction steps]]
  (let [steps-padded (inc steps)]
    (case direction
      :l (mapv
          vector
          (repeat steps-padded row-idx)
          (range col-idx (- col-idx steps-padded) -1))
      :r (mapv
          vector
          (repeat steps-padded row-idx)
          (range col-idx (+ col-idx steps-padded)))
      :u (mapv
          vector
          (range row-idx (- row-idx steps-padded) -1)
          (repeat steps-padded col-idx))
      :d (mapv
          vector
          (range row-idx (+ row-idx steps-padded))
          (repeat steps-padded col-idx)))))

(defn touching?
  "Return true if tail and head touching, no more than 1 step further in
  any direction including diagonal"
  [[tail-row-idx tail-col-idx] [head-row-idx head-col-idx]]
  (and
   (< (abs (- tail-row-idx head-row-idx)) 2)
   (< (abs (- tail-col-idx head-col-idx)) 2)))

(defn tail-step
  "Return new position for the tail given current positions of the tail
  and the head"
  [tail-pos head-pos]
  (let [[tail-row-idx tail-col-idx] tail-pos
        [head-row-idx head-col-idx] head-pos]
    (cond
      ;; tail stays where it is when touching the head
      (touching? tail-pos head-pos) tail-pos
      ;; same row, cols are different
      (and
       (= tail-row-idx head-row-idx)
       (not= tail-col-idx head-col-idx)) [head-row-idx (if (> head-col-idx
                                                              tail-col-idx)
                                                         (inc tail-col-idx)
                                                         (dec tail-col-idx))]
      ;; rows are different, cols are the same
      (and (not= tail-row-idx head-row-idx)
           (= tail-col-idx head-col-idx)) [(if (> head-row-idx tail-row-idx)
                                             (inc tail-row-idx)
                                             (dec tail-row-idx))
                                           head-col-idx]
      ;; rows and cols are different, move diagonally
      :else [(if (> head-row-idx tail-row-idx)
               (inc tail-row-idx)
               (dec tail-row-idx))
             (if (> head-col-idx
                    tail-col-idx)
               (inc tail-col-idx)
               (dec tail-col-idx))])))

(defn tail-path
  "Given tail pos and a sequence of positions that head moved, return a
  sequence of position of the tail to follow the head"
  [tail-pos head-pos-seq]
  (reduce
   (fn [acc head-pos]
     (let [tail-pos-curr (if (empty? acc) tail-pos (last acc))
           tail-pos-new (tail-step tail-pos-curr head-pos)
           stay? (= tail-pos-curr tail-pos-new)]
       ;; when acc is empty, add the very tail-pos to the acc
       ;; to avoid of-by-one error
       (if (and (seq acc) stay?)
         acc
         (conj acc tail-pos-new))))
   []
   head-pos-seq))

(defn line->move
  "Parse a line with direction like: 'L 12' => [:l 12]"
  [s]
  (let [[direction steps] (string/split s #"\s+")
        d (-> direction
              string/lower-case
              keyword)
        s (Integer/parseInt steps)]
    [d s]))

(defn full-head-route
  "Return a sequence of all position that the head steps over according
  to the given moves"
  [head-pos moves]
  (->>
   moves
   (reduce
    (fn [acc single-move]
      (let [curr-pos (if (empty? acc)
                       head-pos
                       (last acc))
            ;; exclude first item as it duplicates
            ;; current head position
            path (rest (head-path curr-pos single-move))]
        ;; (prn single-move path)
        (into acc path)))
    [])))

(defn unique-tail-pos
  "Return number positions that the tail visits at least once"
  []
  (let [lines (-> (tools/input-path)
                  tools/path->lines)
        moves (->> lines
                   (map line->move))
        start-pos [0 0]
        head-route (full-head-route start-pos moves)
        tail-route (tail-path start-pos head-route)
        visited-at-least-once (-> tail-route
                                  set
                                  count)]
    visited-at-least-once))

(defn multi-tail-path
  "Return seq of positions of the last tail of the n-tails for a given
  start position of each tail and the head and given head moves."
  [start-pos moves n-tails]
  (loop [route (full-head-route start-pos moves)
         counter n-tails]
    (if (pos? counter)
      (let [new-route (tail-path start-pos route)]
        (recur new-route (dec counter)))
      route)))

(defn count-tail-pos
  "Return number positions that the last of n-tail visits at least once"
  [n-tails]
  (let [lines (-> (tools/input-path)
                  tools/path->lines)
        moves (->> lines
                   (map line->move))
        start-pos [0 0]
        tail-9th-route (multi-tail-path start-pos moves n-tails)
        visited-at-least-once (-> tail-9th-route
                                  set
                                  count)]
    visited-at-least-once))

(comment
  ;; Part 1 - 5619
  ;; Perf: 1'935 msec, okay-ish
  (unique-tail-pos)

  ;; Part 2 - 2376
  ;; Perf: 5'130 msec, well, could have been better,
  ;; but still 4x faster than in first version
  (count-tail-pos 9))
