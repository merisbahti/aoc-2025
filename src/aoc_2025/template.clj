(ns aoc-2025.template
  (:require
   [clojure.test :refer [is testing deftest]]
   [aoc-2025.core :refer [get-input-for-day]]))

(def input (get-input-for-day))

(defn sol1 [input] nil)
(defn sol2 [input] nil)

(deftest input-tests
  (testing "part 1"
    (is (= (sol1 input) nil))
    (is (= (sol2 input) nil))))
