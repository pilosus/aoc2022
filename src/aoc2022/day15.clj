(ns aoc2022.day15
  "https://adventofcode.com/2022/day/15"
  (:require
   [aoc2022.tools :as tools]
   [clojure.set :as s]))

(defn distance
  "Return Manhattan distance of two pos"
  [p1 p2]
  (let [[r1 c1] p1
        [r2 c2] p2]
    (+ (abs (- r1 r2))
       (abs (- c1 c2)))))

(defn pos-within-distance
  "Given a starting position and a distance, generate a set of all
  possible positions with the same distance of less"
  [pos d]
  (let [[r c] pos
        rows (range (- r d) (+ r d 1))
        cols (range (- c d) (+ c d 1))]
    (into (set [])
          (for [r' rows
                c' cols
                :let [pos' [r' c']
                      d' (distance pos pos')]
                :when (<= d' d)]
            pos'))))

(defn coverage
  "Return a set of positions where no other beacon can be found for the
  given sensor, i.e. set of positions where Manhattance distance
  between a position and a sensor is less or equal to distance between
  the sensor and the beacon."
  [sensor beacon]
  (let [d (distance sensor beacon)]
    (pos-within-distance sensor d)))

(def pos-regex #"(?s).*x=(?<sc>-?\d+), y=(?<sr>-?\d+):.*x=(?<bc>-?\d+), y=(?<br>-?\d+)")

(defn ->int
  [s]
  (Integer/parseInt s))

(defn line->pos
  [line]
  (let [m (re-matcher pos-regex line)]
    (if (.matches m)
      (let [[sc sr bc br] (->> m
                               re-groups
                               rest
                               (map ->int))]
        [[sr sc] [br bc]])
      (throw (Exception. (format "Cannot parse a line: %s" line))))))

(defn occupied
  "Given a seq of all sensor/beacon position pairs, return a flat set of
  positions occupied by either a sensor or a beacon"
  [pos-seq]
  (reduce
   (fn [init pair]
     (let [[sensor beacon] pair]
       (conj init sensor beacon)))
   (set [])
   pos-seq))

(defn row-coverage
  "Find coverage for a given set of coverages and a row number"
  [coverages row]
  (->> coverages
       (filter (fn [[r _]] (= r row)))
       (into #{})))

(defn pos->coverage
  "Given a sequence of [sensor beacon] positions, return a set of all
  coverages for a given row"
  [pos-seq row]
  (let [full (reduce
              (fn [init e]
                (let [[sensor beacon] e
                      full-cov (coverage sensor beacon)
                      row-cov (row-coverage full-cov row)]
                  (s/union init row-cov)))
              (set [])
              pos-seq)
        occ (occupied pos-seq)]
    (s/difference full occ)))

(defn borders
  "Given a seq of all sensor/beacon positions, find max and min rows and cols"
  [pos-seq]
  (let [flat-pos (occupied pos-seq)
        rows (sort-by first flat-pos)
        cols (sort-by second flat-pos)]
    {:row-min (-> rows first first)
     :row-max (-> rows last first)
     :col-min (-> cols first second)
     :col-max (-> cols last second)}))

;; test data

(def tl
  ["Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
   "Sensor at x=9, y=16: closest beacon is at x=10, y=16"
   "Sensor at x=13, y=2: closest beacon is at x=15, y=3"
   "Sensor at x=12, y=14: closest beacon is at x=10, y=16"
   "Sensor at x=10, y=20: closest beacon is at x=10, y=16"
   "Sensor at x=14, y=17: closest beacon is at x=10, y=16"
   "Sensor at x=8, y=7: closest beacon is at x=2, y=10"
   "Sensor at x=2, y=0: closest beacon is at x=2, y=10"
   "Sensor at x=0, y=11: closest beacon is at x=2, y=10"
   "Sensor at x=20, y=14: closest beacon is at x=25, y=17"
   "Sensor at x=17, y=20: closest beacon is at x=21, y=22"
   "Sensor at x=16, y=7: closest beacon is at x=15, y=3"
   "Sensor at x=14, y=3: closest beacon is at x=15, y=3"
   "Sensor at x=20, y=1: closest beacon is at x=15, y=3"])

(comment
  (def tp (->> tl (map line->pos)))
  (def tr 10)
  (def tc (pos->coverage tp tr))
  ;; 26 positions
  (count tc))

(defn positions-without-beacons
  [lines row]
  (let [pos (map line->pos lines)
        cov (pos->coverage pos row)]
    cov))

(defn count-positions
  []
  (let [lines (-> (tools/input-path)
                  tools/path->lines)
        result (positions-without-beacons lines 2000000)]
    (count result)))

;; real data

(comment
  (def lines (-> (tools/input-path)
                 tools/path->lines))
  (def pos (map line->pos lines)))
