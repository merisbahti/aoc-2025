(ns aoc-2025.day6
  (:require
   [aoc-2025.core :refer [get-input-for-day]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def testinput
  "123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +  ")

(def input (get-input-for-day))

(defn sol1 [input] (->>
                    (str/split input #"\n")
                    (map str/trim)
                    (map #(str/split % #"\s+"))
                    (map (fn [items]
                           (map (fn [value]
                                  (cond
                                    (= value "*") *
                                    (= value "+") +
                                    :else (parse-long value))) items)))
                    (reduce (fn [acc curr]
                              (map cons curr acc))
                            (repeat []))
                    (map eval)
                    (apply +)))
(defn sol2 [input] nil)

(deftest input-tests
  (testing "part 1"
    (is (= 4277556 (sol1 testinput)))
    (is (= nil (sol1 input)))
    (is (= nil (sol2 testinput)))
    (is (= nil (sol2 input)))))
