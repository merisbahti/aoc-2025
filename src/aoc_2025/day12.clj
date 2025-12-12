(ns aoc-2025.day12
  (:require
   [aoc-2025.core :refer [get-input-for-day]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def testinput "0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2")
(def input (str/trim (get-input-for-day)))
(filter (partial = \#) (seq "###aa"))

(defn sol1 [input]
  (->> (let [parts (str/split input #"\n\n")
             shape-sizes
             (->> (drop-last parts)
                  (seq)
                  (map #(filter (partial = \#) %))
                  (map count)
                  (into []))
             rows-str (str/split-lines (last parts))
             rows-parsed (map (fn [row-str]
                                (let [[size-unparsed present-counts-unparsed] (str/split row-str #": ")
                                      size (->> (str/split size-unparsed #"x") (map parse-long))
                                      present-counts (->> (str/split present-counts-unparsed #" ") (map parse-long))]

                                  [size present-counts]))
                              rows-str)]

         (->> (map (fn [[[size-x size-y] present-counts]] (>

                                                           (* size-x size-y)
                                                           (->> (map-indexed (fn [i count]
                                                                               (* (shape-sizes i) count)) present-counts)
                                                                (apply +))))

                   rows-parsed)

              (filter identity) (count)))))

(defn sol2 [input] nil)

(deftest input-tests
  (testing "part 1"
    ;; (is (= nil (sol1 testinput)))
    (is (= 505 (sol1 input)))
    (is (= nil (sol2 testinput)))
    (is (= nil (sol2 input)))))
