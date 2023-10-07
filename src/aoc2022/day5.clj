(ns aoc2022.day5
  "https://adventofcode.com/2022/day/5"
  (:require
   [aoc2022.tools :as tools]))

(def stack-storage
  {1 ["B" "W" "N"]
   2 ["L" "Z" "S" "P" "T" "D" "M" "B"]
   3 ["Q" "H" "Z" "W" "R"]
   4 ["W" "D" "V" "J" "Z" "R"]
   5 ["S" "H" "M" "B"]
   6 ["L" "G" "N" "J" "H" "V" "P" "B"]
   7 ["J" "Q" "Z" "F" "H" "D" "L" "S"]
   8 ["W" "S" "F" "J" "G" "Q" "B"]
   9 ["Z" "W" "M" "S" "C" "D" "J"]})

(defn parse-move
  "Parse move string into a sequence of (count, from, to)"
  [s]
  (->> s
       (re-seq #"[0-9]+")
       (map #(Integer/parseInt %))))

(defn move-crates
  "Update storage according to a move"
  [storage move {:keys [reverse]
                 :or {reverse true}}]
  (let [move-fn (if reverse reverse identity)
        [cnt from to] move
        crates-from (drop-last cnt (get storage from))
        crates (take-last cnt (get storage from))
        crates-to (concat (get storage to) (move-fn crates))]
    (assoc storage from crates-from to crates-to)))

(defn move-crates-one-by-one
  [storage move]
  (move-crates storage move {:reverse true}))

(defn move-crates-bulk
  [storage move]
  (move-crates storage move {:reverse false}))

(defn top-crates
  "Return a string with top crates from the given storage"
  [storage]
  (reduce
   (fn [init idx]
     (let [top-crate (last (get storage idx))]
       (str init top-crate)))
   ""
   (range 1 10)))

(defn rearrange-storage
  "Rearrange given storage according to input moves"
  [{:keys [reverse]}]
  (let [move-fn (if reverse move-crates-one-by-one move-crates-bulk)
        lines (-> (tools/input-path) tools/path->lines)
        moves (map parse-move lines)
        storage (reduce move-fn stack-storage moves)]
    (top-crates storage)))

(comment
  ;; Part 1 - MQSHJMWNH
  (rearrange-storage {:reverse true})

  ;; Part 2 - LLWJRBHVZ
  (rearrange-storage {:reverse false}))
