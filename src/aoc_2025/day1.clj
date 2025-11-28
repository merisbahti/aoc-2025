(ns aoc-2025.day1
  (:require [clojure.test :refer [is testing deftest]] [clojure.string :as str] [clojure.java.io :as io] )
)

(def filename (->
               *file*
               (str/split  #"/")
               (last)
               (str/split  #"\.")
               (first)))

(def input (slurp (io/resource "day1.txt")))

(deftest part1-test
  (testing "Part 1"
    (is (= 7 (+ 3 4)))
    (is (= 5 (+ 2 2)))))
