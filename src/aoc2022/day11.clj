(ns aoc2022.day11
  "https://adventofcode.com/2022/day/11"
  (:require
   [aoc2022.aoc2022 :as tools]
   [clojure.core.match :refer [match]]
   [clojure.string :as string]))

(def expr-regex #"(?s)(?<v1>old)\s+(?<op>[\*\+\-])\s+(?<v2>(old|[0-9]*))")
(def op-str->fn {"*" * "+" + "-" -})

(defn str->int
  "Return an integer found anywhere in the given string"
  [s]
  (try (->> s
            (re-find #"[0-9]+")
            (Integer/parseInt))
       (catch Exception _ nil)))

(defn str->int-or-str
  "Parse integer, if fails return original string"
  [s]
  (if (re-matches #"[0-9]+" s)
    (Integer/parseInt s)
    s))

(defn parse-items
  "Return a vector of integers representing starting item levels"
  [starting-items]
  (mapv
   #(Integer/parseInt %)
   (-> starting-items
       (string/split #":\s+")
       last
       (string/split #",\s+"))))

(defn parse-expr
  "Parse expression line into a function"
  [s]
  (let [expr (last (string/split s #"=\s+"))
        m (re-matcher expr-regex expr)]
    (when (.matches m)
      (let [v1 (str->int-or-str (.group m "v1"))
            op (get op-str->fn (.group m "op"))
            v2 (str->int-or-str (.group m "v2"))]
        (match
         [v1 op v2]
          ["old" * "old"] (fn [level] (* level level))
          ["old" * _] (partial * v2)
          ["old" + _] (partial + v2)
          :else (throw (Exception. "Expression cannot be parsed")))))))

(defn parse-monkey
  "Return a map with the parsed data about a monkey"
  [line]
  (let [[monkey-idx
         starting-items
         operation
         test-condition
         true-branch
         false-branch] (-> line (string/split-lines))
        items (parse-items starting-items)
        monkey (str->int monkey-idx)
        op (parse-expr operation)
        test-divisor (str->int test-condition)
        test-true (str->int true-branch)
        test-false (str->int false-branch)]
    {:id monkey
     :items items
     :operation-fn op
     :test-divisor test-divisor
     :test-true test-true
     :test-false test-false
     :inspected 0}))

(defn worry-down
  "Return a lower level of worry once a monkey bored with the item"
  [level]
  (quot level 3))

(defn handle-bigint
  "If a worry level doesn't get lowered as in Part 2, after a few rounds
  levels become too big: BigInteger is used to represent them. All
  operations on such big numbers become very slow.

  One can note that we don't need an exact level value after 10'000
  rounds. The only thing we need is a proper item redirection,
  i.e. division test condition must hold. Too meet the condition, we
  can lower level number by the greatest common divisor of all
  monkey's divisors. Then, calculating a reminder (or a modulo, which
  is the same for positive numbers) (mod big-level gcd) gives us a
  much lower number that still can be used to test divisibility for
  any of the monkeys."
  [gcd level]
  (rem level gcd))

(defn update-level
  [level monkey worry-less?]
  (let [level-up (:operation-fn monkey)
        level-down (if worry-less?
                     worry-down
                     (partial handle-bigint (:gcd monkey)))]
    (-> level
        level-up
        level-down)))

(defn inject-gcd
  [gcd monkey]
  (assoc monkey :gcd gcd))

(defn calc-gcd
  "Calculate greates common divisor for all monkeys.
  NB! Input data contains only prime divisors, that's why simply
  multiplication will do!"
  [monkeys]
  (reduce
   (fn [init monkey]
     (* init (:test-divisor monkey))) 1 monkeys))

(defn init-monkeys
  "Return a vector of inital monkey states"
  ([path]
   (let [monkey-lines (-> path slurp (string/split #"\n{2}"))
         monkeys (mapv parse-monkey monkey-lines)
         gcd (calc-gcd monkeys)]
     (mapv #(inject-gcd gcd %) monkeys)))
  ([]
   (init-monkeys (tools/input-path))))

(defn redirect-item
  "Given test conditions, return monkey index to redirect item to"
  [level monkey]
  (let [{:keys [test-divisor test-true test-false]} monkey]
    (if (= (mod level test-divisor) 0)
      test-true
      test-false)))

(defn process-item
  "Update monkey states as a result of processing a single item"
  [idx item monkeys worry-less?]
  (let [monkey (nth monkeys idx)
        new-level (update-level item monkey worry-less?)
        redirect-idx (redirect-item new-level monkey)
        new-monkeys (-> monkeys
                        (update-in [redirect-idx :items] conj new-level)
                        (update-in [idx :inspected] inc))]
    new-monkeys))

(defn process-items
  "Update monkey states for all items of a given monkey"
  [idx monkeys worry-less?]
  (loop [items (:items (nth monkeys idx))
         state monkeys]
    (if (empty? items)
      (assoc-in state [idx :items] [])
      ;; state
      (let [item (first items)
            new-state (process-item idx item state worry-less?)]
        (recur (next items) new-state)))))

(defn process-monkeys
  "Update monkey states in 1 round"
  [monkeys worry-less?]
  (loop [ms monkeys
         state monkeys]
    (if (empty? ms)
      state
      (let [idx (:id (first ms))
            new-state (process-items idx state worry-less?)]
        (recur (next ms) new-state)))))

(defn process-rounds
  "Update monkey states for given number of rounds"
  [{:keys [monkeys rounds worry-less?]}]
  (loop [iterations (range rounds)
         state monkeys]
    (if (empty? iterations)
      state
      (recur (next iterations) (process-monkeys state worry-less?)))))

(defn monkey-business
  "Return a product of number of times two most active monkeys inspected
  items"
  [monkeys]
  (->> monkeys
       process-rounds
       (map #(get % :inspected))
       (sort >)
       (take 2)
       (apply *)))

(comment
  (let [monkeys (init-monkeys)]
    ;; Part 1 - 54253 - 5 ms
    (monkey-business {:monkeys monkeys :rounds 20 :worry-less? true})
    ;; Part 2 - 13119526120 - 580 ms
    (monkey-business {:monkeys monkeys :rounds 10000 :worry-less? false})))
