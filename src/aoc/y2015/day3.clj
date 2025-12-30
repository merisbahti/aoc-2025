(ns aoc.y2015.day3
  (:require
   [aoc.core :refer [get-input-for-day]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def testinput1 "^v")
(def testinput2 "^>v<")
(def testinput3 "^v^v^v^v^v")
(def input (get-input-for-day))

(defn sol1 [input] nil)
(defn sol2 [input] nil)

(deftest input-tests
  (testing "part 1"
    (is (= 2 (sol1 testinput1)))
    (is (= 4 (sol1 testinput2)))
    (is (= 2 (sol1 testinput3)))
    (is (= nil (sol1 input)))
    (is (= 3 (sol2 testinput1)))
    (is (= 3 (sol2 testinput2)))
    (is (= 11 (sol2 testinput3)))
    (is (= nil (sol2 input)))))
