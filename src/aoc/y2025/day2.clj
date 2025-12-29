(ns aoc.y2025.day2
  (:require
   [clojure.string :as str]
   [clojure.test :refer [is testing deftest]]
   [aoc.core :refer [get-input-for-day]]))

(def input (get-input-for-day))
(def testinput "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124")

(defn sol1 [input]
  (->> (str/split input #"\s*,\s*")
       (mapcat
        (fn [range-str]
          (let
           [[min-value max-value]
            (map
             (comp parse-long str/trim)
             (str/split range-str #"-"))]
            (range min-value (+ 1 max-value)))))
       (filter (fn [num] (let* [strnum (str num)
                                strlen (count strnum)
                                half-strlen (/ strlen 2)
                                is-even (= 0 (mod strlen 2))]
                               (and is-even
                                    (=
                                     (take half-strlen strnum)
                                     (take half-strlen (drop half-strlen strnum)))))))
       (apply +)))

(defn sol2 [input]
  (->> (str/split input #"\s*,\s*")
       (mapcat
        (fn [range-str]
          (let
           [[min-value max-value]
            (map
             (comp parse-long str/trim)
             (str/split range-str #"-"))]
            (range min-value (+ 1 max-value)))))
       (filter
        (fn [row] (re-matches #"(.*)\1+" (str row))))
       (apply +)))

(deftest input-tests
  (testing "part 1"
    (is (= (sol1 testinput) 1227775554))
    (is (= (sol1 input) 24157613387))
    (is (= (sol2 testinput) 4174379265))
    (is (= (sol2 input) 33832678380))))
