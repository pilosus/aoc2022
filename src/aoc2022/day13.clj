(ns aoc2022.day13
  "https://adventofcode.com/2022/day/13"
  (:require
   [aoc2022.tools :as tools]
   [clojure.string :as string]))

(defn cmp-int
  "Compare integer values"
  [left right]
  (cond
    (< left right) true
    (= left right) nil
    (> left right) false))

(defn ordered?
  "Return true if packets are ordered, false otherwise"
  [left right]
  (cond
    (and (int? left) (int? right)) (cmp-int left right)
    (int? left) (ordered? [left] right)
    (int? right) (ordered? left [right])
    :else (let [results
                (loop [ls (seq left)
                       rs (seq right)
                       acc []]
                  (cond
                    (and (empty? ls) (seq rs)) (conj acc true)
                    (and (seq ls) (nil? rs)) (conj acc false)
                    (and (empty? ls) (empty? rs)) acc
                    :else (let [l (first ls)
                                r (first rs)
                                sub (ordered? l r)]
                            (recur (next ls) (next rs) (conj acc sub)))))]
            (->> results
                 (filter boolean?)
                 first))))

(defn obj->packets
  "Given a string of two vectors, return a seq of two parsed vectors"
  [line]
  (->> line
       string/split-lines
       (map read-string)))

(defn sum-ordered-indices
  "Return a sum of packets indices that are ordered. First index is 1."
  [packets]
  (->> packets
       (map (fn [[l r]] (ordered? l r)))
       (map-indexed vector)
       (filter (fn [[_ v]] (true? v)))
       (map first)
       (map inc)
       (apply +)))

(defn sum-of-indicies
  "Return a sum of indices for ordered packets for the given input"
  []
  (let [obj (-> (tools/input-path)
                tools/path->objects)
        packets (->> obj
                     (map obj->packets))

        result (sum-ordered-indices packets)]
    result))

(comment
  ;; Part 1 - 5330
  (sum-of-indicies))
