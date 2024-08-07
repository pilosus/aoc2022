(ns aoc2022.day16
  "https://adventofcode.com/2022/day/16"
  (:require
   [aoc2022.tools :as tools]
   [clojure.string :as str]))

(def valve-regex #"(?s)Valve (?<valve>[A-Z]{2}) has flow rate=(?<rate>[0-9]+); tunnels? leads? to valves? (?<children>.*)")

(defn parse-line
  "Parse a single input line into a vector"
  [s]
  (let [m (re-matcher valve-regex s)]
    (when (.matches m)
      (let [valve (.group m "valve")
            rate (Integer/parseInt (.group m "rate"))
            children (str/split (.group m "children") #", ")]
        [valve {:rate rate :children children}]))))

(def graph-test
  {"AA" {:rate 0 :children ["DD" "II" "BB"]}
   "BB" {:rate 13 :children ["CC" "AA"]}
   "CC" {:rate 2 :children ["DD" "BB"]}
   "DD" {:rate 20 :children ["CC" "AA" "EE"]}
   "EE" {:rate 3 :children ["FF" "DD"]}
   "FF" {:rate 0 :children ["EE" "GG"]}
   "GG" {:rate 0 :children ["FF" "HH"]}
   "HH" {:rate 22 :children ["GG"]}
   "II" {:rate 0 :children ["AA" "JJ"]}
   "JJ" {:rate 21 :children ["II"]}})

(defn backtrace
  "Given a mapping of node-parent, return a path from one node to another"
  [parents from to]
  (let [trace-fn
        (fn [parents from to path]
          (cond
            (= from to) (rseq path)
            (nil? to) nil
            :else
            (let [parents' (dissoc parents from)
                  to' (get parents to)
                  path' (conj path to')]
              (recur parents' from to' path'))))]
    (trace-fn parents from to [to])))

(defn shortest-path-bfs
  "Return a shortest path in graph from one node to another one.
  Breadth-First Search (BFS) algorithm is used for the search."
  [graph from to]
  (loop [queue [from]
         parents {from nil}]
    (if (empty? queue)
      (backtrace parents from to)
      (let [current (first queue)]
        (if (= current to)
          (backtrace parents from to)
          (let [[queue' parents']
                (loop [neighbours (get-in graph [current :children])
                       queue'' queue
                       parents'' parents]
                  (if (empty? neighbours)
                    [queue'' parents'']
                    (let [neighbour (first neighbours)]
                      ;; Inner loop recur
                      (if (contains? parents'' neighbour)
                        (recur (next neighbours) queue'' parents'')
                        (recur
                         (next neighbours)
                         (conj queue'' neighbour)
                         (assoc parents'' neighbour current))))))]
            ;; Outer loop recur
            (recur
             (into [] (next queue'))
             parents')))))))

(defn working-valves
  "Return a list of valves that can create pressure"
  [graph]
  (->> graph
       (filter (fn [[k v]] (> (:rate v) 0)))
       keys))

(defn get-dist
  "Return a mapping from node to do to a distance between the nodes"
  [graph]
  (let [valves (conj (working-valves graph) "AA")
        result (reduce
                (fn [acc item]
                  (assoc
                   acc item
                   (into
                    {}
                    (map
                     (fn [v]
                       (let [length
                             (->> v
                                  (shortest-path-bfs graph item)
                                  count
                                  dec)]
                         [v length]))
                     (remove #(= % item) valves)))))
                {}
                valves)]
    result))

(defn dfs
  "Depth-First Search to find maximum pressure under time constraints"
  [graph time valve visited]
  (let [distances (get-dist graph)]
    (loop [stack [[time valve visited 0]]
           pressure-max 0]
      (if (empty? stack)
        pressure-max
        (let [[time valve visited pressure] (peek stack)
              stack (pop stack)
              pressure-max (max pressure-max pressure)]
          (recur
           (reduce
            (fn [accumulator [neighbor distance]]
              (if (contains? visited neighbor)
                accumulator
                (let [time-processing 1
                      time-remaining (- time distance time-processing)
                      visited' (conj visited neighbor)
                      pressure' (+ pressure (* (get-in graph [neighbor :rate]) time-remaining))]
                  (if (<= time-remaining 0)
                    accumulator
                    (conj accumulator [time-remaining neighbor visited' pressure'])))))
            stack
            (get distances valve))
           pressure-max))))))

(defn part1
  "Part 1"
  []
  (let [lines (-> (tools/input-path)
                  tools/path->lines)
        graph (into {} (map parse-line lines))
        ;; 1737
        result (dfs graph 30 "AA" #{})]
    result))
