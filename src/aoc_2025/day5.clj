(ns aoc-2025.day5
  (:require
   [aoc-2025.core :refer [get-input-for-day]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def testinput "3-5
10-14
16-20
12-18

1
5
8
11
17
32")
(def input (get-input-for-day))

(defn sol1 [input]
  (let [[unparsed-ranges unparsed-numbers]
        (str/split input #"\n\n")
        numbers (map parse-long (str/split unparsed-numbers #"\n"))
        ranges
        (->>
         (str/split unparsed-ranges #"\n")
         (map (fn [x]
                (let
                 [matcher (re-matcher #"(?<start>\d+)-(?<end>\d+)" x)]
                  (if (.matches matcher)
                    {:start (parse-long (.group matcher 1))
                     :end (parse-long (.group matcher 2))})))))]
    (->>
     (filter
      (fn [number]
        (some
         (fn [{start :start end :end}]
           (and (>= number start) (<= number end))) ranges)) numbers)
     count)))
(defn sol2 [input] nil)

(deftest input-tests
  (testing "part 1"
    (is (= 3 (sol1 testinput)))
    (is (= 567 (sol1 input)))
    (is (= nil (sol2 testinput)))
    (is (= nil (sol2 input)))))
