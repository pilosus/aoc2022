(ns aoc2022.day4
  "https://adventofcode.com/2022/day/4"
  (:require
   [aoc2022.aoc2022 :as tools]
   [clojure.string :as string]))

(def p1 "36-92,40-78") ;; contains
(def p2 "36-92,10-78") ;; doesn't contain

(defn range-str->int
  [s]
  (let [[ls rs] (string/split s #"\-")]
    [(Integer/parseInt ls)
     (Integer/parseInt rs)]))

(defn contain-fully?
  "Return true if one of the ranges contains another fully.
  Ranges are passed in as a string of form: r1l-r1r,r2l-r2r, e.g. 1-10,2-8.
  Examples: 1-12 contains 5-9 fully, but 1-10,5-15 don't overlap fully."
  [s]
  (let [[r1, r2] (string/split s #",")
        [range-1-left range-1-right] (range-str->int r1)
        [range-2-left range-2-right] (range-str->int r2)]
    ;; (prn range-1-left range-1-right range-2-left range-2-right)
    (or
      ;; range 1 contains fully range 2
     (and (<= range-1-left range-2-left)
          (>= range-1-right range-2-right))
      ;; range 2 contains fully range 1
     (and (<= range-2-left range-1-left)
          (>= range-2-right range-1-right)))))

(defn overlap?
  "Return true if one of the ranges overlaps the other one.
  Ranges are passed in as a string of form: r1l-r1r,r2l-r2r, e.g. 1-10,2-8.
  Examples: 1-10 overlaps 4-6, but 1-10,30-40 don't overlap"
  [s]
  (let [[r1, r2] (string/split s #",")
        [range-1-left range-1-right] (range-str->int r1)
        [range-2-left range-2-right] (range-str->int r2)]
    ;; (prn range-1-left range-1-right range-2-left range-2-right)
    (or
      ;; range 1 overlaps range 2
     (and (<= range-1-left range-2-left)
          (>= range-1-right range-2-left))
      ;; range 2 overlaps range 1
     (and (<= range-2-left range-1-left)
          (>= range-2-right range-1-left)))))

(defn total-fully-contained
  "Return total count of ranges where one range fully contains the other"
  []
  (let [ranges (-> (tools/input-path) tools/path->lines)
        total (->> ranges
                   (map contain-fully?)
                   (filter true?)
                   count)]
    total))

(defn total-overlaps
  "Return total count of ranges where one range overlaps the other"
  []
  (let [ranges (-> (tools/input-path) tools/path->lines)
        total (->> ranges
                   (map overlap?)
                   (filter true?)
                   count)]
    total))

(comment
  ;; Part 1 - 494
  (total-fully-contained)
  ;; Part 2 - 833
  (total-overlaps))
