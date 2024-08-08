(ns aoc2022.day18
  "https://adventofcode.com/2022/day/18"
  (:require
   [aoc2022.tools :as tools]
   [clojure.string :as str]))

(def SIDES-EXPOSED 6)

(defn parse-line
  [s]
  (let [ints (-> s (str/split #","))]
    (into [] (map (fn [v] (Integer/parseInt v)) ints))))

(defn parse-lines
  [lines]
  (reduce
   (fn [acc line]
     (assoc acc (parse-line line) SIDES-EXPOSED))
   {}
   lines))

(defn adjacents
  "Return a set of cube that can be adjacent to a given one"
  [[x y z]]
  (let [right [(inc x) y z]
        left [(dec x) y z]
        up [x (inc y) z]
        down [x (dec y) z]
        forth [x y (inc z)]
        back [x y (dec z)]]
    #{right left up down forth back}))

(defn count-neighbours
  "For a given cube count number of the adjacent neighbour cubes"
  [all-cubes cube]
  (->> cube
       adjacents
       (clojure.set/intersection all-cubes)
       count))

(defn update-exposure
  "Update cubes sides exposure in the data"
  [data]
  (let [all-cubes (into #{} (keys data))]
    (reduce
     (fn [acc [cube sides-exposed]]
       (let [sides-covered (count-neighbours all-cubes cube)]
         (assoc acc cube (- sides-exposed sides-covered))))
     {}
     data)))

(defn calculate-exposure
  "Caclulate number of exposed cube sides"
  [data]
  (->> data
       vals
       (apply +)))

(defn part1
  "Part 1"
  [& {:keys [test?] :or {test? false}}]
  (let [result (->> (tools/input-path test?)
                    tools/path->lines
                    parse-lines
                    update-exposure
                    calculate-exposure)]
    result))
