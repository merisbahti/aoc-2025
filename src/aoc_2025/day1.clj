(ns aoc-2025.day1
  (:require
   [clojure.string :as str]
   [clojure.test :refer [is testing deftest]]
   [aoc-2025.core :refer [get-input-for-day]]))

(def input (get-input-for-day))
(def testinput "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")

(defn sol1 [input] (->> (str/split input #"\n")
                        (map
                         (fn [row]
                           (let [[_ polarity count] (re-find (re-matcher #"([LR])(\d+)" row))]
                             (* (if (= polarity "R") 1 -1) (parse-long count)))))

                        (reduce (fn [[head & tail] curr]
                                  (cons (mod (+ curr head) 100) (cons head tail))) [50])
                        (filter (partial = 0)) (count)))

(defn sol2 [input] nil)

(deftest input-tests
  (testing "part 1"
    (is (= 3 (sol1 testinput)))
    (is (= 1105 (sol1 input)))
    (is (= (sol2 testinput) nil))
    (is (= (sol2 input) nil))))
