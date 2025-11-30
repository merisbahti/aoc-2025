(ns aoc-2025.day1
  (:require
   [clojure.test :refer [is testing deftest]]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [aoc-2025.core :refer [get-input-for-day filename]]))

(def input (get-input-for-day))
(comment (get-input-for-day))

(deftest part1-test
  (testing "Part 1"
    (is (= 7 (+ 3 4)))
    (is (= 5 (+ 2 3)))))
