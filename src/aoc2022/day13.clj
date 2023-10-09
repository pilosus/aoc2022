(ns aoc2022.day13
  "https://adventofcode.com/2022/day/13"
  (:require
   [aoc2022.tools :as tools]
   [clojure.string :as string]))

(defn non-zero?
  "Return true of value is not a 0 and not nil"
  [v]
  (and (not= v 0) (some? v)))

(defn ordered?
  "Return true if packets are ordered, false otherwise"
  [left right]
  (cond
    (and (int? left) (int? right)) (compare left right)
    (int? left) (ordered? [left] right)
    (int? right) (ordered? left [right])
    :else (let [results
                (loop [ls (seq left)
                       rs (seq right)
                       acc []]
                  (cond
                    (and (empty? ls) (seq rs)) (conj acc -1)
                    (and (seq ls) (nil? rs)) (conj acc 1)
                    (and (empty? ls) (empty? rs)) acc
                    :else (let [l (first ls)
                                r (first rs)
                                sub (ordered? l r)]
                            (recur (next ls) (next rs) (conj acc sub)))))]
            (->> results
                 (filter non-zero?)
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
       (filter (fn [[_ v]] (neg? v)))
       (map first)
       (map inc)
       (apply +)))

(defn sum-of-ordered-packet-indicies
  "Return a sum of indices of the ordered packets"
  []
  (let [obj (-> (tools/input-path)
                tools/path->objects)
        packets (->> obj
                     (map obj->packets))

        result (sum-ordered-indices packets)]
    result))

(defn product-of-divider-indices
  "Return a product of divider indices in a sequence of the packets
  sorted so that they are all ordered correctly"
  []
  (let [divider-1 [[2]]
        divider-2 [[6]]
        obj (-> (tools/input-path)
                tools/path->objects)
        packets (->> obj
                     (map obj->packets)
                     (reduce
                      (fn [init [l r]]
                        (conj init l r))
                      [])
                     (cons divider-1)
                     (cons divider-2))
        sorted (sort ordered? packets)
        divider-1-idx (->> divider-1
                           (.indexOf sorted)
                           inc)
        divider-2-idx (->> divider-2
                           (.indexOf sorted)
                           inc)
        result (* divider-1-idx divider-2-idx)]
    result))

(comment
  ;; Part 1 - 5330
  (sum-of-ordered-packet-indicies)
  ;; Part 2 - 27648
  (product-of-divider-indices))
