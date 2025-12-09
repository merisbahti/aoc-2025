(ns aoc-2025.template
  (:require
   [aoc-2025.core :refer [get-input-for-day]]
   [clojure.string :as str]

   [clojure.test :refer [deftest is testing]]
   [clojure.math.combinatorics :as combo]))

(def testinput "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3")

(def input (get-input-for-day))

(->> input
     (str/split-lines)
     (map #(str/split % #","))
     (map (fn [[x y]] [(parse-long x) (parse-long y)]))
     (#(combo/combinations % 2))
     (map (fn [[[x1 y1] [x2 y2]]]
            (*
             (+ 1 (abs (- x1 x2)))
             (+ 1 (abs (- y1 y2))))))
     (apply max))

(defn sol1 [input] nil)
(defn sol2 [input] nil)

(deftest input-tests
  (testing "part 1"
    (is (= nil (sol1 testinput)))
    (is (= nil (sol1 input)))
    (is (= nil (sol2 testinput)))
    (is (= nil (sol2 input)))))
