(ns aoc2022.day8
  "https://adventofcode.com/2022/day/8"
  (:require
   [aoc2022.tools :as tools]))

(defn matrix-indexed
  "Return a matrix (vector of vectors) with each element indexed
  as [[row-idx col-idx] elem], ..."
  [matrix]
  (reduce
   (fn [result [row-idx heights]]
     (conj
      result
      (reduce
       (fn [row [col-idx height]]
         (conj row [[row-idx col-idx] height]))
       []
       (map-indexed vector heights))))
   []
   (map-indexed vector matrix)))

(defn rows->cols
  "Convert given matrix rows into columns"
  [matrix]
  (into
   []
   (for [row-idx (range (count matrix))]
     (into []
           (for [col-idx (range (count (first matrix)))]
             (get-in matrix [col-idx row-idx]))))))

(defn matrix->all-rows
  "Return a vector of all possible rows and columns from left to right,
  right to left, top to bottom, bottom to top of the given indexed matrix"
  [mi]
  (let [rows-left->right mi
        rows-right->left (mapv #(into [] (reverse %)) mi)
        cols-top->down (rows->cols mi)
        cols-down->top (mapv #(into [] (reverse %)) cols-top->down)]
    (concat rows-left->right
            rows-right->left
            cols-top->down
            cols-down->top)))

(defn filter-visible
  "Return a vector of tree indices that are visible from left to right.
  Input vector of trees must be of form: [[idx1 height1] [idx2 height2]...]"
  [tree-row]
  (loop [vs (seq tree-row)
         tallest -1
         coords []]
    (if (empty? vs)
      coords
      (let [[idx height] (first vs)]
        ;; (prn (format "idx: %s, height: %s, tallest: %s" idx height tallest))
        (if (> height tallest)
          (recur (next vs) height (conj coords idx))
          (recur (next vs) tallest coords))))))

(defn matrix->visible-coords
  "Return a set of all coords of the given indexed matrix"
  [mi]
  (let [rows (->> mi
                  matrix->all-rows
                  (map filter-visible))
        coords (reduce
                (fn [init v] (into init v))
                (set [])
                rows)]
    coords))

(defn count-visible-trees
  "Retur number of visible trees for the input file"
  []
  (let [lines (-> (tools/input-path)
                  tools/path->lines)
        matrix (->> lines
                    (map tools/str->vec)
                    (mapv #(mapv tools/str->int %))
                    matrix-indexed)
        visible-coords (matrix->visible-coords matrix)]
    (count visible-coords)))

;; Part 2

(defn calc-score
  "Return scenery score for a given tree height and other heights"
  [height values]
  (loop [vs (seq values)
         seen 0]
    (if (empty? vs)
      seen
      (let [curr (first vs)]
        (if (>= curr height)
          (inc seen)
          (recur (next vs) (inc seen)))))))

(defn- update-scores
  "If old value doesn't exist, return new, otherwise multiply old and new"
  [old-val new-val]
  (if old-val
    (* old-val new-val)
    new-val))

(defn row-scores
  "Return a vector of maps of coord to scenery score for a given row
  Trees are represented as [index height]"
  [tree-row]
  (loop [vs (seq tree-row)
         coord->score {}]
    (if (empty? vs)
      coord->score
      (let [[idx height] (first vs)
            tail (->> vs rest (map second))
            score (calc-score height tail)]
        (recur
         (next vs)
         (update coord->score idx
                 update-scores
                 score))))))

(defn matrix->scores
  "Return a map of coordinate to scenery score"
  [mi]
  (let [score-maps
        (->> mi
             matrix->all-rows
             (map row-scores))
        scores (apply merge-with update-scores score-maps)]
    scores))

(defn max-scenery-score
  "Return the maximum scenery score for the input"
  []
  (let [lines (-> (tools/input-path)
                  tools/path->lines)
        scores (->> lines
                    (map tools/str->vec)
                    (mapv #(mapv tools/str->int %))
                    matrix-indexed
                    matrix->scores)
        max-val (apply max (vals scores))]
    max-val))

(comment
  ;; Part 1 - 1672
  (count-visible-trees)
  ;; Part 2 - 327180
  (max-scenery-score))
