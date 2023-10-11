(ns aoc2022.day14
  "https://adventofcode.com/2022/day/14"
  (:require
   [aoc2022.tools :as tools]
   [clojure.string :as string]))

;; Input to matrix

(defn ranges->pos
  "Given two ranges, generate a vector of positions"
  [x-range y-range]
  (reduce
   (fn [init [idx x]]
     (let [y (nth y-range idx)]
       (conj init [x y])))
   []
   (map-indexed vector x-range)))

(defn pos->range
  "Given from and start positions, generate a vector of all positions"
  [[x1 y1] [x2 y2]]
  (let [x-from (min x1 x2)
        x-to (max x1 x2)
        y-from (min y1 y2)
        y-to (max y1 y2)]
    (if
     (= x-from x-to)
      (let [y-range (range y-from (inc y-to))
            x-range (repeat (count y-range) x-from)]
        (ranges->pos x-range y-range))
      (let [x-range (range x-from (inc x-to))
            y-range (repeat (count x-range) y-from)]
        (ranges->pos x-range y-range)))))

(defn x-y->from-start
  "x,y coords in the input mean col,row in the Clojure matrix, that's
  why we need to flip x,y with reverse to get to Clojure's expected
  behavior as in (get-in [row col] matrix)"
  [xy]
  (let [xy-pair (-> xy (string/split #","))]
    (->> xy-pair
         (map #(Integer/parseInt %))
         reverse)))

(defn from-start->pos
  "Given string representation of from and to positionsm return a vector
  of all possible positions"
  [from-pos to-pos]
  (let [from (x-y->from-start from-pos)
        to  (x-y->from-start to-pos)]
    (pos->range from to)))

(defn points->rock-pos
  "Given a vector of string points like ['x1,y1' 'x2,y2', ...], return a
  set of [x y] positions representing rocks"
  [points]
  (loop [ps (seq points)
         result (set [])]
    (if (< (count ps) 2)
      result
      (let [[from to] (take 2 ps)
            pos (from-start->pos from to)]
        (recur (next ps) (into result pos))))))

(defn lines->rocks
  "Return a set of unique [x y] position pairs representing rocks"
  [lines]
  (->> lines
       (map #(string/split % #" -> ")) ;; vectors or string-points
       (reduce ;; vectors of positions (x y pairs)
        (fn [init points]
          (into init (points->rock-pos points)))
        (set []))))

;; Sand mechanics

(defn find-void-floor
  "The void floor is a lowest row of rocks"
  [rocks]
  (let [gr (group-by first rocks)
        lowest-row (->> gr
                        keys
                        (apply max))]
    lowest-row))

(defn overflow?
  "The easiest way to check if the position is in the void, is to check
  if it's below the lowest possible rock row."
  [pos tiles]
  (let [{:keys [floor]} tiles
        [row _] pos]
    (> row floor)))

(defn not-in?
  [tiles pos]
  (let [{:keys [blocks floor infinite-floor]} tiles
        [row _] pos]
    (cond
      (and (= row floor) infinite-floor) false
      (contains? blocks pos) false
      :else true)))

(defn full?
  [pos tiles]
  (let [{:keys [pouring infinite-floor]} tiles]
    (and infinite-floor (= pos pouring))))

(defn sand-next-pos
  "Return next position of the sand and the flag if it's blocked"
  [pos tiles]
  (let [[row col] pos
        down [(inc row) col]
        down-left [(inc row) (dec col)]
        down-right [(inc row) (inc col)]]
    (cond
      (overflow? pos tiles) :overflow
      (not-in? tiles down) down
      (not-in? tiles down-left) down-left
      (not-in? tiles down-right) down-right
      (full? pos tiles) :full
      :else :block)))

(defn move-sand
  [sand-pos tiles]
  (loop [pos sand-pos
         ts tiles]
    (let [next-pos (sand-next-pos pos ts)]
      (cond
        (= next-pos :overflow) (assoc tiles :status :overflow)
        (= next-pos :full) (-> tiles
                               (assoc :status :full)
                               (update :blocks conj pos))
        (= next-pos :block) (update tiles :blocks conj pos)
        :else (recur next-pos ts)))))

(defn sand-flow
  "Update tiles until void reached"
  [tiles]
  (loop [iter 0
         ts tiles]
    (cond
      (= (:status ts) :overflow) (assoc ts :iter (dec iter))
      (= (:status ts) :full) (assoc ts :iter iter)
      :else (recur (inc iter) (move-sand (:pouring ts) ts)))))

(defn init-tiles
  "Return an initial tiles map"
  ([rocks]
   (init-tiles {:infinite-floor false} rocks))
  ([opts rocks]
   (let [{:keys [infinite-floor]} opts
         void (find-void-floor rocks)
         floor (if infinite-floor (+ 2 void) void)]
     {:blocks rocks
      :floor floor
      :infinite-floor infinite-floor
      :pouring [0 500]
      :status :running
      :iter 0})))

;; Entrypoint

(defn count-until-overflow
  "Return number of sands to settle before the overflow"
  [opts]
  (let [lines (-> (tools/input-path)
                  tools/path->lines)
        flow (->> lines
                  lines->rocks
                  (init-tiles opts)
                  sand-flow)]
    (:iter flow)))

(comment
  ;; Part 1 - 817 - 87ms
  (count-until-overflow {:infinite-floor false})
  ;; Part 2 - 23416 - 2400ms
  (count-until-overflow {:infinite-floor true}))
