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

(defn parse-lines-vec
  [lines]
  (reduce
   (fn [acc line]
     (conj acc (parse-line line)))
   []
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
       (let [exposed (- sides-exposed (count-neighbours all-cubes cube))]
         (assoc acc cube exposed)))
     {}
     data)))

(defn calculate-exposure
  "Caclulate number of exposed cube sides"
  [data]
  (->> data
       vals
       (apply +)))

(defn part1
  "Part 1: (part1)
  Test: (part1 :test? true)"
  [& {:keys [test? trapped?] :or {test? false trapped? false}}]
  (let [result (-> (tools/input-path test?)
                   tools/path->lines
                   parse-lines
                   update-exposure
                   calculate-exposure)]
    result))

;; Part 2

(defn find-surface
  [all-cubes]
  [[(dec (apply min (map (fn [v] (nth v 0)) all-cubes)))
    (dec (apply min (map (fn [v] (nth v 1)) all-cubes)))
    (dec (apply min (map (fn [v] (nth v 2)) all-cubes)))]
   [(inc (apply max (map (fn [v] (nth v 0)) all-cubes)))
    (inc (apply max (map (fn [v] (nth v 1)) all-cubes)))
    (inc (apply max (map (fn [v] (nth v 2)) all-cubes)))]])

(defn in-space?
  [cube surface]
  (let [[[min-x min-y min-z] [max-x max-y max-z]] surface
        [nx ny nz] cube]
    (and (>= nx min-x) (<= nx max-x)
         (>= ny min-y) (<= ny max-y)
         (>= nz min-z) (<= nz max-z))))

(defn in?
  "Return true if a collection contains a given element"
  [coll element]
  (some #(= element %) coll))

(defn dfs
  [cubes]
  (let [surface (find-surface cubes)
        [[min-x min-y min-z] [max-x max-y max-z]] surface]
    (loop [stack [[max-x max-y max-z]]
           seen #{}
           exposed 0]
      (if (empty? stack)
        exposed
        (let [cube (peek stack)
              stack' (pop stack)]
          (cond
            (in? cubes cube) (recur stack' seen (inc exposed))
            (not (in? seen cube)) (recur
                                   (reduce
                                    (fn [acc neighbor]
                                      (if (in-space? neighbor surface)
                                        (conj acc neighbor)
                                        acc))
                                    stack'
                                    (adjacents cube))
                                   (conj seen cube)
                                   exposed)
            :else (recur stack' seen exposed)))))))

(defn part2
  "Part 2: (part2)
  Test: (part2 :test? true)"
  [& {:keys [test?] :or {test? false}}]
  (let [cubes (-> (tools/input-path test?)
                  tools/path->lines
                  parse-lines-vec)]
    (dfs cubes)))
