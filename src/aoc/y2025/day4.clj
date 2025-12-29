(ns aoc.y2025.day4
  (:require
   [aoc.core :refer [get-input-for-day]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def testinput "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")
(def input (get-input-for-day))

(def neighbor-deltas
  (for [dx (range -1 2)
        dy (range -1 2)
        :when (not= [dx dy] [0 0])]
    [dx dy]))

(defn count-neighbors [[x y] full]
  {:pre [(set? full)]}
  (->> neighbor-deltas
       (filter
        (fn [[dx dy]]
          (full [(+ x dx) (+ y dy)])))
       count))

(comment (count-neighbors [0 0] (into #{} [[0 1] [0 2] [0 -1] [0 0]]))
         (count-neighbors [0 0] (into #{} [[-1 -1] [0 -1] [1 -1]
                                           [-1 0] [0 0] [1 0]
                                           [-1 1] [0 1] [1 1]])))

(defn sol1 [input]
  (as->
   (->> (str/split input #"\n")
        (map #(str/split % #""))
        (map-indexed
         (fn [y row]
           (map-indexed (fn [x col] (when (= "@" col) [x y])) row)))
        (reduce into #{})
        (filter (complement nil?))
        (set)) x
    (map #(count-neighbors % x) x)
    (filter #(< % 4) x)
    (count x)))

(defn sol2 [input]
  (as->
   (->> (str/split input #"\n")
        (map #(str/split % #""))
        (map-indexed
         (fn [y row]
           (map-indexed (fn [x col] (when (= "@" col) [x y])) row)))
        (reduce into #{})
        (filter (complement nil?))
        (set)) board

    (loop [board-acc board
           changed true]
      (if changed
        (let [new-board
              (->> board-acc
                   (filter
                    (fn [pos]
                      (>= (count-neighbors pos board-acc) 4)))
                   set)]
          (recur new-board (not= new-board board-acc)))
        (- (count board) (count board-acc))))))

(deftest input-tests
  (testing "part 1"
    (is (=  13 (sol1 testinput)))
    (is (=  1428 (sol1 input)))
    (is (=  43 (sol2 testinput)))
    (is (=  8936 (sol2 input)))))
