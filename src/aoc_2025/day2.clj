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

; given abcdef should give '(a b c d e f) '(ab cd ef) '(abc def) '(abcd ef)
(defn all-string-permutations [input]
  (let*  [inputstr (str input)
          half-len (long (/ (count inputstr) 2))
          all-lengths (range 1 (+ 1 half-len))]
         (map
          (fn [len]
            (->> (take-while
                  #(< % (count inputstr))
                  (map-indexed (fn [x i] (* i x)) (repeat len)))
                 (map (fn [start-index] (apply str (take len (drop start-index inputstr)))))))
          all-lengths)))

(defn sol2 [input]
  (->> (str/split input #"\s*,\s*")
       (map (fn [range] (let [[min max] (str/split range #"-")] [(parse-long (str/trim min)) (parse-long (str/trim max))])))
       (map (fn [[min max]] (when (or (not (number? min)) (not (number? max))) (throw "hello")) (range min (+ 1 max))))
       (flatten)
       (filter
        (fn [row] (some (partial apply =) (all-string-permutations row))))
       (apply +)))

(deftest input-tests
  (testing "part 1"
    (is (= (sol1 testinput) 1227775554))
    (is (= (sol1 input) 24157613387))
    (is (= (sol2 testinput) 4174379265))
    (is (= (sol2 input) 33832678380))))
