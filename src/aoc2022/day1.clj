(ns aoc2022.day1
  "https://adventofcode.com/2022/day/1"
  (:require
   [aoc2022.aoc2022 :as tools]
   [clojure.string :as string]))

(defn elfs-with-calories
  "Take in a path to the input, return a hashmap of elf index to total
  calories consumed. Index is zero-based number of the elf in original
  input list."
  []
  (let [elfs-str (-> (tools/input-path)
                     slurp
                     (string/split #"\n{2}"))
        elfs (->> elfs-str
                  (map #(string/split % #"\n{1}"))
                  (reduce
                   (fn [init v]
                     (conj
                      init
                      (->>
                       v
                       (map #(Integer/parseInt %))
                       (reduce + 0))))
                   [])
                  (map-indexed vector)
                  (into {}))]
    elfs))

(defn elf-with-most-calories
  "Take in a hashmap of elf index to calories, return a
  of the elf with the most calories consumed and its calories"
  [elfs]
  (last (apply max-key val elfs)))

(defn top-calories
  []
  (let [elfs (elfs-with-calories)]
    (elf-with-most-calories elfs)))

(defn total-calories-by-top-elfs
  "Take in a hashmap of elf index to calories, return total number of
  calories consumed by N elfs with the most calories consumed"
  [elfs n]
  (->> elfs
       (sort-by val >)
       (take n)
       (reduce (fn [init v] (+ init (last v))) 0)))

(defn total-calories-by-top-3-elfs
  []
  (let [elfs (elfs-with-calories)]
    (total-calories-by-top-elfs elfs 3)))

(comment
  ;; Part 1 - 69528
  (top-calories)
  ;; Part 2 - 206152
  (total-calories-by-top-3-elfs))
