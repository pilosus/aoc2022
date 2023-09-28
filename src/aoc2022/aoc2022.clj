(ns aoc2022.aoc2022
  (:require [clojure.string :as string]))

(defn path->lines
  "Return a vector of string read from the file"
  [path]
  (-> path
      slurp
      string/split-lines))

(defn input-path
  "Return a path to input file for the current namespace"
  []
  (let [day (-> *ns*
                str
                (string/split #"\.")
                last)]
    (format "resources/input/%s.txt" day)))
