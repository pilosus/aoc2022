(ns aoc2022.day15
  "https://adventofcode.com/2022/day/15"
  (:require
   [aoc2022.tools :as tools]))

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

(defn distance
  "Return Manhattan distance of two pos"
  [p1 p2]
  (let [[r1 c1] p1
        [r2 c2] p2]
    (+ (abs (- r1 r2))
       (abs (- c1 c2)))))

(defn interval
  "Return an interval [from-col, to-col] for a given row that covers pos
  within the given Manhattan distance"
  [pos d row]
  (let [[r c] pos]
    (if (or (< row (- r d)) (> row (+ r d)))
      nil
      (let [from (->> (range (- c d) (+ c 1))
                      (take-while
                       (fn [c']
                         (let [d' (distance pos [row c'])]
                           (>= d' d))))
                      last)
            to (->> (range c (+ c d 1))
                    (drop-while
                     (fn [c']
                       (let [d' (distance pos [row c'])]
                         (< d' d))))
                    first)]
        [from to]))))

(defn merge-intervals
  "Given a sequnce of intervals [from, to], return a vector of merged
  intervals sorted by the start column.
  Time complexity: O(n log n) + O(n) = O(n log n)"
  [intervals]
  (let [is (->> intervals
                (remove nil?)
                (sort-by first)
                vec)]
    (loop [intervals' is
           result []]
      (if (empty? intervals')
        result
        (let [[from-prev to-prev] (last result)
              [from-curr to-curr] (first intervals')]
          (if (and from-prev (>= to-prev from-curr))
            (recur (next intervals')
                   (conj (->> result
                              butlast
                              vec)
                         [from-prev (max to-curr to-prev)]))
            (recur (next intervals') (conj result [from-curr to-curr]))))))))

(defn crop-by-limits
  "Return an interval cropped by given limits if needed"
  [interval limits]
  (let [[from-i to-i] interval
        [from-l to-l] limits
        from (if (>= from-i from-l) from-i from-l)
        to (if (<= to-i to-l) to-i to-l)]
    [from to]))

(defn poss->intervals
  "Given a sequence of [sensor beacon] positions and a row, return a
  vector of intervals where no other beacons can be found"
  [poss row limits]
  (let [merged (->> poss
                    (reduce
                     (fn [acc [sensor beacon]]
                       (let [d (distance sensor beacon)]
                         (conj acc (interval sensor d row))))
                     [])
                    merge-intervals)]
    (if limits
      (mapv #(crop-by-limits % limits) merged)
      merged)))

(defn occupied-by-row
  "Return a map of row numbers to a vector of sensor and beacon pos"
  [poss]
  (->> poss
       (reduce
        (fn [acc pair]
          (let [[sensor beacon] pair]
            (conj acc sensor beacon)))
        [])
       (group-by first)))

(defn count-occupied
  "Count number of positions occupied by either a sensor or a beacon for
  a given row"
  [occupied-map row]
  (-> occupied-map
      (get row [])
      set
      count))

(defn- count-pos-in-intervals
  "Count positions in a single interval [from, to]"
  [iv]
  (let [[from to] iv]
    (- (inc to) from)))

(defn count-pos
  "Count positions in a sequence of intervals"
  [intervals]
  (reduce
   (fn [acc ii]
     (+ (count-pos-in-intervals ii) acc))
   0
   intervals))

(defn count-unvailable-pos
  "Count positions unavailable for the beacons for a given row"
  [lines row opts]
  (let [{:keys [limits exclude-occupied?]} opts
        poss (mapv line->pos lines)
        intervals (poss->intervals poss row limits)
        total (count-pos intervals)
        result (if exclude-occupied?
                 (- total (count-occupied (occupied-by-row poss) row))
                 total)]
    result))

(defn first-available-pos
  "Find first position available for a beacon"
  [lines limits]
  (let [[from to] limits
        expected (count-pos [limits])
        poss (mapv line->pos lines)
        found (loop [rows (range from (inc to))]
                (if rows
                  (let [row (first rows)
                        intervals (poss->intervals poss row limits)
                        cnt (count-pos intervals)]
                    (if (< cnt expected)
                      [row intervals]
                      (recur (next rows))))
                  nil))]
    found))

(comment
  (let [tlines ["Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
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
                "Sensor at x=20, y=1: closest beacon is at x=15, y=3"]
        lines (-> (tools/input-path)
                  tools/path->lines)]

    ;; Part 1 - 5176944 - 1700 ms
    ;; test
    (count-unvailable-pos tlines 10 {:exclude-occupied? true})
    ;; prod
    (count-unvailable-pos lines 2000000 {:exclude-occupied? true})

    ;; Part 2
    ;; test
    (first-available-pos tlines [0 20])))
