(ns aoc2022.tools
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

(defn str->vec
  "Return a vector of letters"
  [s]
  (string/split s #""))

(defn str->int
  "Parse string into an integer"
  [s]
  (Integer/parseInt s))
