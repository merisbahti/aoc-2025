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

(defn sol1 [input] (->> (str/split input #"\n")
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
(defn sol2 [input] nil)

(deftest input-tests
  (testing "part 1"
    (is (= 21 (sol1 testinput)))
    (is (= 1546 (sol1 input)))
    (is (= nil (sol2 testinput)))
    (is (= nil (sol2 input)))))
