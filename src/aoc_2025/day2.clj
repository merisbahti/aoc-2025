(ns aoc-2025.template
  (:require
   [clojure.string :as str]
   [clojure.test :refer [is testing deftest]]
   [aoc-2025.core :refer [get-input-for-day]]))

(def input (get-input-for-day))
(def testinput "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124")

(defn sol1 [input]
  (->> (str/split input #"\s*,\s*")
       (map (fn [range] (let [[min max] (str/split range #"-")] [(parse-long (str/trim min)) (parse-long (str/trim max))])))
       (map (fn [[min max]] (when (or (not (number? min)) (not (number? max))) (throw "hello")) (range min (+ 1 max))))
       (flatten)
       (filter (fn [num] (let* [strnum (str num)
                                strlen (count strnum)
                                half-strlen (/ strlen 2)
                                is-even (= 0 (mod strlen 2))]
                               (and is-even
                                    (=
                                     (take half-strlen strnum)
                                     (take half-strlen (drop half-strlen strnum)))))))
       (apply +)))

(defn sol2 [input] nil)

(deftest input-tests
  (testing "part 1"
    (is (= 1227775554 (sol1 testinput)))
    (is (= (sol1 input) 24157613387))
    (is (= (sol2 input) nil))))
