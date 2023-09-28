(ns aoc2022.day3
  "https://adventofcode.com/2022/day/3"
  (:require
   [aoc2022.aoc2022 :as tools]
   [clojure.string :as string]
   [clojure.set :refer [intersection]]))

(defn letter->priority
  "Retrun a hashmaps of letters priorities letter=>priority"
  []
  (let [lower (string/split "abcdefghijklmnopqrstuvwxyz" #"")
        letters (concat lower (map string/upper-case lower))
        priorities (->> letters
                        (map-indexed (fn [idx elem] [elem (inc idx)]))
                        (into {}))]
    priorities))

(defn sum-priorities
  "Return the sum of priorities for common items in the substrings"
  [rucksack-str]
  (let [priorities (letter->priority)
        rucksack (string/split rucksack-str #"")
        [c1 c2] (map set (split-at (/ (count rucksack) 2) rucksack))
        common-items (intersection c1 c2)
        total (->> common-items
                   (map #(get priorities %))
                   (reduce + 0))]
    total))

(defn total-priorities-rucksacks-with-common-items
  "Return the total sum of all priorities for rucksacks with common
  items in two compartments"
  []
  (let [rucksacks (-> (tools/input-path) tools/path->lines)
        priorities (->> rucksacks
                        (map sum-priorities)
                        (reduce + 0))]
    priorities))

(defn- str->set [s]
  (-> s
      (string/split #"")
      set))

(defn total-priorities-rucksack-groups
  "Return the total sum of all priorities for group-of-three rucksacks
  common items"
  []
  (let [rucksacks (-> (tools/input-path) tools/path->lines)
        priorities (letter->priority)
        groups (partition 3 rucksacks)
        total (->> groups
                   (map
                    (fn [grp]
                      (let [common-items-in-group
                            (apply intersection (map str->set grp))]
                        (->> common-items-in-group
                             (map #(get priorities %))
                             (reduce + 0)))))
                   (reduce + 0))]
    total))

(comment
  ;; Part 1
  (total-priorities-rucksacks-with-common-items)

  ;; Part 2
  (total-priorities-rucksack-groups))
