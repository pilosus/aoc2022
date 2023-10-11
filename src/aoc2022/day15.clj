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

;; TODO for large distances ranges for columns may be huge. Keeping
;; them in a set may be ineffective! Given that the range is
;; continious monotonic sequence of integers, we may want to keep
;; only [from, to). Then the task will boil down to:
;; - finding a union of all intervals (interval/segment tree?)
;; - removing column numbers of given sensors and beacons from the overlap
(defn cols-within-distance
  "Return a set of columns of positions for a given row that are within
  the given Manhattan distance of the given position"
  [pos d row]
  (let [[r c] pos
        row-out-of-range? (or (< row (- r d)) (> row (+ r d)))]
    (if row-out-of-range?
      (set [])
      (reduce
       (fn [init c']
         (let [pos' [row c']
               d' (distance pos pos')]
           (if (<= d' d)
             (conj init c')
             init)))
       (set [])
       (range (- c d) (+ c d 1))))))

(defn occupied-cols
  [pos row]
  (let [[r c] pos]
    (if (= r row)
      #{c}
      #{})))

(defn coverage
  "Return a set of positions where no other beacon can be found for the
  given sensor, i.e. set of positions where Manhattance distance
  between a position and a sensor is less or equal to distance between
  the sensor and the beacon."
  [sensor beacon row]
  (let [d (distance sensor beacon)
        cols (cols-within-distance sensor d row)]
    (s/difference
     cols
     (occupied-cols sensor row)
     (occupied-cols beacon row))))

(def pos-regex #"(?s).*x=(?<sc>-?\d+), y=(?<sr>-?\d+):.*x=(?<bc>-?\d+), y=(?<br>-?\d+)")

(defn ->int
  "Parse string into integer"
  [s]
  (Integer/parseInt s))

(defn line->pos
  "Parse a line of input into a vector of two pos for a sensor and a beacon"
  [line]
  (let [m (re-matcher pos-regex line)]
    (if (.matches m)
      (let [[sc sr bc br] (->> m
                               re-groups
                               rest
                               (map ->int))]
        [[sr sc] [br bc]])
      (throw (Exception. (format "Cannot parse a line: %s" line))))))

(defn pos->coverage
  "Given a sequence of [sensor beacon] positions, return a set of all
  coverages for a given row"
  [pos-seq row]
  (reduce
   (fn [init e]
     (let [[sensor beacon] e
           cov (coverage sensor beacon row)]
       (s/union init cov)))
   (set [])
   pos-seq))

;; test data

(comment
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

  (def tp (->> tl (map line->pos)))
  (def tr 10)
  (def tc (pos->coverage tp tr))
  ;; 26 positions
  (count tc)

  ;; check perf
  (time (cols-within-distance [2000469 3999724] 562976 2000000))
  (time (cols-within-distance [8733 3995530] 1375195 2000000)))

(defn count-positions
  "Return number of pos in a given row where no beacons can be found"
  []
  (let [lines (-> (tools/input-path)
                  tools/path->lines)
        pos (map line->pos lines)
        row 2000000
        cov (pos->coverage pos row)]
    (count cov)))

(comment
  ;; Part 1 - 5176944 - 8900 ms
  (count-positions))
