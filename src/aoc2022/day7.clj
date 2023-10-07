(ns aoc2022.day7
  "https://adventofcode.com/2022/day/7"
  (:require
   [aoc2022.tools :as tools]
   [clojure.string :as string]))

(def command-regex #"(?s)\$\s+(?<cmd>(\w+))\s*(?<dir>(.*))")
(def listing-regex #"(?s)(?<size>([0-9]+))?(?<dir>(dir))?\s+(?<name>(.*))")

(defn line->cmd
  "Return a hashmap with the parsed output line"
  [l]
  (let [cmd (re-matcher command-regex l)
        ls (re-matcher listing-regex l)]
    (cond
      (.matches cmd)
      (let [cmd-name (keyword (.group cmd "cmd"))
            arg-name (.group cmd "dir")]
        {:cmd {cmd-name arg-name}})
      (.matches ls)
      (let [tp (if (.group ls "dir") :d :f)
            sz (.group ls "size")
            size (if sz (Integer/parseInt sz) 0)
            name (.group ls "name")]
        (if (= tp :f)
          {:out {:f size}}
          {:out {:d name}}))
      :else {})))

(defn tree-map
  "Return a map of path to vector of sizes. Vector may contain integers
  representing file sizes or strings referencing relative paths in the
  tree."
  [lines]
  (loop [commands (map line->cmd lines)
         path []
         tree {}]
    (if (empty? commands)
      tree
      (let [cmd (first commands)
            [path-upd tree-upd]
            (cond
              (= (get-in cmd [:cmd :cd]) "..")
              [(pop path) tree]

              (= (get-in cmd [:cmd :cd]) "/")
              [[""] tree]

              (get-in cmd [:cmd :cd])
              [(conj path (get-in cmd [:cmd :cd])) tree]

              (get-in cmd [:out :d])
              (let [dir-name (get-in cmd [:out :d])
                    tree-key (string/join "/" path)
                    tree-val (get tree tree-key [])]
                ;; add reference to the dir
                [path (assoc tree tree-key (conj tree-val dir-name))])

              (get-in cmd [:out :f])
              (let [size (get-in cmd [:out :f])
                    tree-key (string/join "/" path)
                    tree-val (get tree tree-key [])]
                ;; add size
                [path (assoc tree tree-key (conj tree-val size))])

              :else
              [path tree])]
        (recur (next commands) path-upd tree-upd)))))

(defn path->sizes
  "Return a vector of integers representing file sizes for a given path"
  [path paths result]
  (loop [sizes (get paths path)
         flat-result []]
    (if (empty? sizes)
      (concat result flat-result)
      (let [size (first sizes)]
        (if (int? size)
          ;; file size
          (recur (next sizes) (conj flat-result size))
          ;; reference to another path
          (let [path-new (str path "/" size)]
            (recur
             (next sizes)
             (concat
              flat-result
              (path->sizes path-new paths [])))))))))

(defn dirs-map
  "Return a map of a dir path to its total size"
  [lines]
  (let [paths-map (tree-map lines)]
    (loop [paths (keys paths-map)
           result {}]
      (if (empty? paths)
        result
        (let [path (first paths)
              sizes (path->sizes path paths-map [])
              total (reduce + 0 sizes)]
          (recur (next paths) (assoc result path total)))))))

(defn total-dirs-size
  "Return total size of all dirs under given limit of size"
  [limit]
  (let [lines (-> (tools/input-path) tools/path->lines)
        dirs (->> lines
                  dirs-map
                  (filter (fn [[_ size]] (<= size limit))))
        total (reduce (fn [init [_ size]]
                        (+ init size)) 0 dirs)]
    total))

(defn smallest-dir-to-remove
  "Return the smallest directory that will free up enough space on disk"
  [update-space]
  (let [lines (-> (tools/input-path) tools/path->lines)
        dirs (->> lines dirs-map)
        total-disk-space 70000000
        unused-space (- total-disk-space (get dirs ""))
        free-up (- update-space unused-space)
        [_ size] (->> dirs
                      (filter (fn [[_ size]] (>= size free-up)))
                      (sort-by second)
                      first)]
    size))

(comment
  ;; Part 1 - 1501149
  (total-dirs-size 100000)
  ;; Part 2 - 10096985
  (smallest-dir-to-remove 30000000))
