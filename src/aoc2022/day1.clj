(ns aoc2022.day1
  "https://adventofcode.com/2022/day/1"
  (:require [clojure.string :as string]))

(def INPUT-PATH "resources/input/day1.txt")

(defn elfs-with-calories
  "Take in a path to the input, return a hashmap of elf index to total
  calories consumed. Index is zero-based number of the elf in original
  input list."
  [path]
  (let [elfs-str (-> path
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
  "Take in a hashmap of elf index to calories, return a vector of index
  of the elf with the most calories consumed and its calories"
  [elfs]
  (apply max-key val elfs))

(defn total-calories-by-top-elfs
  "Take in a hashmap of elf index to calories, return total number of
  calories consumed by N elfs with the most calories consumed"
  [elfs n]
  (->> elfs
       (sort-by val >)
       (take n)
       (reduce (fn [init v] (+ init (last v))) 0)))

(comment
  (let [elfs (-> INPUT-PATH
                 elfs-with-calories)]
    (prn "Part 1" (last (elf-with-most-calories elfs)))
    (prn "Part 2" (total-calories-by-top-elfs elfs 3))))
