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
  [pos lowest-row]
  (let [[row _] pos]
    (> row lowest-row)))

(defn not-in?
  [coll k]
  (not (contains? coll k)))

(defn sand-next-pos
  "Return next position of the sand and the flag if it's blocked"
  [pos tiles]
  (let [{:keys [blocks floor]} tiles
        [row col] pos
        down [(inc row) col]
        down-left [(inc row) (dec col)]
        down-right [(inc row) (inc col)]]
    (cond
      (overflow? pos floor) :void
      (not-in? blocks down) down
      (not-in? blocks down-left) down-left
      (not-in? blocks down-right) down-right
      :else :block)))

(defn move-sand
  [sand-pos tiles]
  (loop [pos sand-pos
         ts tiles]
    (let [next-pos (sand-next-pos pos ts)]
      (cond
        (= next-pos :void) (assoc tiles :status :void)
        (= next-pos :block) (update tiles :blocks conj pos)
        :else (recur next-pos ts)))))

(defn sand-flow
  "Update tiles until void reached"
  [tiles]
  (loop [iter 0
         ts tiles]
    (if (= (:status ts) :void)
      ;; Iterations before void reached, hence (dec iter)
      (assoc ts :iter (dec iter))
      (recur (inc iter) (move-sand (:pouring ts) ts)))))

(defn init-tiles
  "Return an initial tiles map"
  [rocks]
  (let [floor (find-void-floor rocks)]
    {:blocks rocks
     :floor floor
     :pouring [0 500]
     :status :ok
     :iter 0}))

(comment
  ;; Test data
  (let [ls ["498,4 -> 498,6 -> 496,6"
            "503,4 -> 502,4 -> 502,9 -> 494,9"]
        rs (lines->rocks ls)
        ts (init-tiles rs)
        result (sand-flow ts)]
    result))

;; Entrypoint

(defn count-until-overflow
  "Return number of sands to settle before the overflow"
  []
  (let [lines (-> (tools/input-path)
                  tools/path->lines)
        flow (->> lines
                  lines->rocks
                  init-tiles
                  sand-flow)]
    (:iter flow)))

(comment
  ;; Part 1 - 817 - 87ms
  (count-until-overflow))
