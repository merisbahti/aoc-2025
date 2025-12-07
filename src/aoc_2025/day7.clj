(ns aoc-2025.day7
  (:require
   [aoc-2025.core :refer [get-input-for-day]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def testinput ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............")

(def input (get-input-for-day))

(defn sol1 [input]
  (->> (str/split input #"\n")
       (map
        (fn [row]
          (->> (str/split row #"")
               (map-indexed (fn [i x] [i x]))
               (filter (fn [[_ x]] (or  (= x "S") (= x "^")))))))
       (reduce (fn [{tachyons :tachyons split-beams :split-beams} row]
                 (assert (every?

                          (fn [[_ x]]
                            (or (= x "S") (= x  "^")))
                          row))
                 (assert (every? number? tachyons))
                 (let [splitter-indexes (set (map first row))
                       new-split-tachyons
                       (count (filter (fn [tachyon] (splitter-indexes tachyon)) tachyons))
                       new-tachyons
                       (concat
                        (mapcat (fn [[i x]] (if (= "S" x) [i] []))
                                row)
                        (mapcat (fn [tachyon-index]
                                  (cond
                                    (splitter-indexes tachyon-index)
                                    [(- tachyon-index 1) (+ tachyon-index 1)]
                                    :else [tachyon-index]))

                                tachyons))]

                   {:split-beams (+ split-beams new-split-tachyons) :tachyons (set new-tachyons)}))

               {:split-beams 0 :tachyons #{}})
       (:split-beams)))

(def count-paths
  (memoize (fn  [col row graph]
             (assert (number? col))
             (let [next-row (get   graph (+  1 row))
                   left (-  col 1)
                   right (+  col 1)
                   next-row-index (+ 1 row)]
               (cond
                 (nil? next-row) 1
                 (next-row col)
                 (count-paths col next-row-index graph)
                 (and (next-row left) (next-row right))
                 (do
                   (+
                    (count-paths left next-row-index graph)
                    (count-paths right next-row-index graph)))
                 :else (do
                         (print col row next-row)

                         (assert false)))))))
(defn sol2 [input]
  (->> (str/split input #"\n")
       (map
        (fn [row]
          (->> (str/split row #"")
               (map-indexed (fn [i x] [i x]))
               (filter (fn [[_ x]] (or  (= x "S") (= x "^")))))))
       (reduce
        (fn [[tachyons :as rest-acc] row]
          (assert (every?

                   (fn [[_ x]]
                     (or (= x "S") (= x  "^")))
                   row))
          (assert (every? number? tachyons))

          (let [splitter-indexes (set (map first row))
                new-tachyons
                (concat
                 (mapcat (fn [[i x]] (if (= "S" x) [i] []))
                         row)
                 (mapcat (fn [tachyon-index]
                           (cond
                             (splitter-indexes tachyon-index)
                             [(- tachyon-index 1) (+ tachyon-index 1)]
                             :else [tachyon-index]))

                         tachyons))]

            (if
             (not= (set new-tachyons) tachyons)
              (cons (set new-tachyons) rest-acc)
              rest-acc)))

        [#{}])
       (reverse)
       (drop 1)
       (into [])
       ((fn [graph]
          (println "is graph" graph)
          (count-paths (first (first graph)) 0 graph)))))

(time (count-paths 7 1 [#{}
                        #{7}
                        #{6 8}
                        #{7 9 5}
                        #{4 6 10 8}
                        #{7 3 11 9 5 8}
                        #{7 4 6 12 2 10 8}
                        #{7 1 4 13 3 11 5 10 8}
                        #{0 4 6 12 2 11 14 10 8}]))

(deftest input-tests
  (testing "part 1"
    (is (= 21 (sol1 testinput)))
    (is (= 1546 (sol1 input)))
    (is (= 40 (sol2 testinput)))
    (is (= 13883459503480 (sol2 input)))))
