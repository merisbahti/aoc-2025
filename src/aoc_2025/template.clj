(ns aoc-2025.template
  (:require
   [aoc-2025.core :refer [get-input-for-day]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def testinput "")
(def input (get-input-for-day))

(defn sol1 [input] nil)
(defn sol2 [input] nil)

(deftest input-tests
  (testing "part 1"
    (is (= (sol1 testinput) nil))
    (is (= (sol1 input) nil))
    (is (= (sol2 testinput) nil))
    (is (= (sol2 input) nil))))
