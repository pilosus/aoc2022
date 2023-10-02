(ns aoc2022.day8
  "https://adventofcode.com/2022/day/8"
  (:require
   [aoc2022.aoc2022 :as tools]))

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

(comment
  ;; Part 1 - 1672
  (count-visible-trees)
  ;; Part 2 -
  )
