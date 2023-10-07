(ns aoc2022.day10
  "https://adventofcode.com/2022/day/10"
  (:require
   [aoc2022.tools :as tools]
   [clojure.string :as string]))

;; Part 1 - Signal strength

(defn line->cmd
  [l]
  (let [[cmd v] (string/split l #"\s+")
        command (keyword cmd)
        value (if (= command :addx) (Integer/parseInt v) 0)
        cycles (if (= command :addx) 2 1)]
    [command value cycles]))

(defn generate-register-states
  "Return a vector of register state at any given cycle. Cycle number
  equals to theq vector index. Cycle numbers start from 1"
  [init command-seq]
  (loop [commands command-seq
         register init
         states [init]]
    (if (empty? commands)
      (conj states register)
      (let [[_ v cycles] (first commands)
            new-register (+ register v)
            new-states (->> register
                            (repeat cycles)
                            (into states))]
        (recur (next commands) new-register new-states)))))

(defn signal-strength
  "Return signal strength for a vector or register states"
  [register-states]
  (reduce
   (fn [acc idx]
     (let [v (nth register-states idx)
           strength (* v idx)]
       (+ acc strength)))
   0
   (range 20 (count register-states) 40)))

(defn calc-total-strength
  "Return total signal strength for the given list of commands"
  []
  (let [lines (-> (tools/input-path)
                  tools/path->lines)
        commands (->> lines
                      (map line->cmd))
        total-strength (->> commands
                            (generate-register-states 1)
                            signal-strength)]
    total-strength))

;; Part 2 - screen rendering

(defn pos->pixel
  "Given CRT relative position [0..39] and 3px wide sprite center
  position [0 39] return a pixel being drawn"
  [crt-pos sprite-pos]
  (let [overlaps? (and (>= crt-pos (dec sprite-pos))
                       (<= crt-pos (inc sprite-pos)))]
    (if overlaps?
      "#"
      ".")))

(defn sprites->crt
  "Given sprite positions and the CRT size, return a string representing
  drawn CRT lines of pixels"
  [sprites crt-length crt-height]
  (let [pixels (loop [crt-positions (range (* crt-length crt-height))
                      row 1
                      result []]
                 (if (empty? crt-positions)
                   result
                   (let [crt-pos (first crt-positions)
                         crt-relative-pos (mod crt-pos crt-length)
                         sprite-pos (nth sprites crt-pos)
                         pixel (pos->pixel crt-relative-pos sprite-pos)]
                     (recur (next crt-positions)
                            (inc row)
                            (conj result pixel)))))
        lines (->> pixels
                   (partition crt-length)
                   (map #(string/join "" %)))
        crt (string/join "\n" lines)]
    crt))

(defn render-screen
  "Return a string representing rendered screen"
  []
  (let [lines (-> (tools/input-path)
                  tools/path->lines)
        register-cycles (->> lines
                             (map line->cmd)
                             (generate-register-states 1))
        sprites (->> register-cycles
                     rest
                     butlast)
        crt (sprites->crt sprites 40 6)]
    crt))

(comment
  ;; Part 1 - 13720
  (calc-total-strength)
  ;; Part 2 - FBURHZCH
  (print (render-screen)))
