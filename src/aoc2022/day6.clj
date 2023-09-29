(ns aoc2022.day6
  "https://adventofcode.com/2022/day/6"
  (:require
   [aoc2022.aoc2022 :as tools]))

(defn unique-seq?
  [pairs n]
  (let [letters (mapv second pairs)]
    (and (= (count letters) (count (set letters)))
         (= (count letters) n))))

(defn update-buffer
  "Return sliding buffer of size n updated with value v"
  [buffer v n]
  (let [tail (into [] (rest buffer))]
    (if (>= (count buffer) n)
      (conj tail v)
      (conj buffer v))))

(defn find-seq-start
  "Find index a character in a given string after a sequence of n unique chars"
  [s n]
  (let [letters-indexed (map-indexed vector (tools/str->vec s))
        [idx _] (loop [letters (seq letters-indexed)
                       buffer []]
                  (cond
                    (unique-seq? buffer n) (last buffer)
                    (and (not (unique-seq? buffer n))
                         (empty? letters)) (throw
                                            (Exception. "Sequence not found!"))
                    :else (let [pair (first letters)
                                buffer-updated (update-buffer buffer pair n)]
                            (recur (next letters) buffer-updated))))]
    (+ idx 1)))

(defn find-packet-start
  "Find index for the first char after start-of-packet marker"
  []
  (let [signal (-> (tools/input-path)
                   tools/path->lines
                   first)]
    (find-seq-start signal 4)))

(defn find-message-start
  "Find index for the first char after start-of-message marker"
  []
  (let [signal (-> (tools/input-path)
                   tools/path->lines
                   first)]
    (find-seq-start signal 14)))

(comment
  ;; Part 1 - 1833
  (find-packet-start)

  ;; Part 2 - 3425
  (find-message-start))
