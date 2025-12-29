(ns aoc.y2025.template
  (:require
   [aoc.core :refer [get-input-for-day]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def testinput "")
(def input (get-input-for-day))

(defn sol1 [input] nil)
(defn sol2 [input] nil)

(deftest input-tests
  (testing "part 1"
    (is (= nil (sol1 testinput)))
    (is (= nil (sol1 input)))
    (is (= nil (sol2 testinput)))
    (is (= nil (sol2 input)))))
