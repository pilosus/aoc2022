(ns aoc2022.day11
  "https://adventofcode.com/2022/day/11"
  (:require
   [aoc2022.aoc2022 :as tools]
   [clojure.math :refer [floor]]
   [clojure.core.match :refer [match]]
   [clojure.string :as string]))

(defn str->sym
  "Parse string into operator, integer or a string"
  [s]
  (cond (contains? #{"+" "-" "*" "/"} s) (-> s symbol resolve)
        (re-matches #"[0-9]+" s) (Integer/parseInt s)
        :else s))

;; TODO rewrite with macro?
(defn parse-op
  "Return a function for processing worry level"
  [s]
  (let [parts (string/split s #"\s+")
        [v1 op v2] (map str->sym parts)]
    (match
     [v1 op v2]
      ["old" _ "old"] (fn [n] (op n n))
      ["old" _ _] (fn [n] (op n v2))
      [_ _ "old"] (fn [n] (op v1 n)))))

(defn str->int
  "Return an integer found in the given string"
  [s]
  (->> s
       (re-find #"[0-9]+")
       (Integer/parseInt)))

(defn line->monkey
  "Return a map with the parsed data about a monkey"
  [line]
  (let [[monkey-num
         starting-items
         operation
         test-condition
         true-branch
         false-branch] (-> line (string/split #"\n"))
        items (mapv
               #(Integer/parseInt %)
               (-> starting-items
                   (string/split #":\s+")
                   last
                   (string/split #",\s+")))
        monkey (str->int monkey-num)
        op (-> operation
               (string/split #"=\s+")
               last
               parse-op)
        divisable-by (str->int test-condition)
        true-monkey (str->int true-branch)
        false-monkey (str->int false-branch)
        level->monkey (fn [level]
                        (if (= (mod level divisable-by) 0)
                          true-monkey
                          false-monkey))]
    {:monkey monkey
     :items items
     :worry-up-fn op
     :redirect-fn level->monkey
     :inspected 0}))

(defn worry-down
  "Return a lower level of worry once a monkey bored with the item"
  [level]
  (-> level
      (/ 3)
      floor
      int))

(defn init-monkeys
  "Return a vector of inital monkey states"
  ([path]
   (let [monkey-lines (-> path slurp (string/split #"\n{2}"))]
     (mapv line->monkey monkey-lines)))
  ([]
   (init-monkeys (tools/input-path))))

(defn process-items
  [idx monkeys]
  (let [monkey (nth monkeys idx)
        worry-up (:worry-up-fn monkey)
        redirect (:redirect-fn monkey)]
    (loop [items (:items monkey)
           states monkeys]
      (if (empty? items)
        states
        (let [level (first items)
              new-level (-> level
                            worry-up
                            worry-down)
              redirect-idx (redirect new-level)
              new-states (-> states
                             (update-in [redirect-idx :items] conj new-level)
                             (update-in [idx :inspected] inc)
                             (update-in [idx :items]
                                        (fn [old] (into [] (rest old)))))]
          ;; can an item be redirected to themselves?
          (recur (next items) new-states))))))

(defn process-rounds
  "Return a state of monkeys after processing n rounds"
  [monkeys rounds]
  (if (pos? rounds)
    (let [updated-monkeys
          (loop [indices (range (count monkeys))
                 result monkeys]
            (if (empty? indices)
              result
              (let [idx (first indices)]
                (recur (next indices) (process-items idx result)))))]
      (process-rounds updated-monkeys (dec rounds)))
    monkeys))

(defn monkey-business
  "Return a product of number of times two most active monkeys inspected
  items"
  []
  (let [monkeys (process-rounds (init-monkeys) 20)
        inspected (sort > (map #(get % :inspected) monkeys))
        total (* (first inspected) (second inspected))]
    total))

(comment
  ;; Part 1 - 54253
  (monkey-business))
