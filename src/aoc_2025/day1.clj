(ns aoc-2025.day1
  (:require
   [clojure.test :refer [is testing deftest]]
   [aoc-2025.core :refer [get-input-for-day]]))

(def input (get-input-for-day))

(deftest part1-test
  (testing "Part 1"
    (is (= 7 (+ 3 4)))
    (is (= 5 (+ 2 3)))))
