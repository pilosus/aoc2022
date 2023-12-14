(ns aoc2022.tools
  (:require [clojure.string :as string]))

(def alphabet "abcdefghijklmnopqrstuvwxyz")

(defn letters
  []
  (string/split alphabet #""))

(defn letter->idx
  ([]
   (letter->idx 0))
  ([starting-idx]
   (let [ls (letters)]
     (zipmap ls (iterate inc starting-idx)))))

(defn path->lines
  "Return a vector of string read from the file"
  [path]
  (-> path
      slurp
      string/split-lines))

(defn path->line
  "Return a string read from the file"
  [path]
  (-> path
      slurp
      string/trim))

(defn path->objects
  "Return a vector of string read from the file with objects separated
  by a blank line"
  [path]
  (-> path slurp (string/split #"\n{2}")))

(defn input-path
  "Return a path to input file for the current namespace"
  []
  (let [day (-> *ns*
                str
                (string/split #"\.")
                last)]
    (format "resources/input/%s.txt" day)))

(defn str->vec
  "Return a vector of letters"
  [s]
  (string/split s #""))

(defn str->int
  "Parse string into an integer"
  [s]
  (Integer/parseInt s))

;; Vectors

(defn ->v
  [s]
  (into [] s))

;; Matrix ops

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
