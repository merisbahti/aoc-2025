(ns aoc-2025.day3
  (:require
   [aoc-2025.core :refer [get-input-for-day]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))
(def testinput "987654321111111
811111111111119
234234234234278
818181911112111
")
(def input (get-input-for-day))

(filter
 #(> (:index %) 0)
 '({:number 9, :index 1} {:number 8, :index 0}))
(defn highest-joltage [input-string]
  (let* [indexed (->>
                  (str/split input-string #"")
                  (map-indexed (fn [i x] {:number (parse-long x) :index i})))
         sorted-by-number (sort-by (comp - :number) indexed)]
        (loop [curr-list sorted-by-number]
          (assert (not (empty? curr-list)))

          (let* [curr-head (first curr-list)
                 remaining-list   (filter #(> (:index %) (:index curr-head)) sorted-by-number)
                 second-highest-number (first remaining-list)]

                (if second-highest-number
                  (parse-long (str (:number curr-head) (:number second-highest-number)))
                  (recur (rest curr-list)))))))
(deftest input-tests
  (testing "part 1"
    (is (=  99 (highest-joltage "991234")))
    (is (=  89 (highest-joltage "89")))
    (is (=  89 (highest-joltage "819")))
    (is (=  11 (highest-joltage "1111")))))

(comment (highest-joltage "123495"))

(defn sol1 [input] nil)
(defn sol2 [input] nil)

(deftest input-tests
  (testing "part 1"
    (is (=  nil (sol1 testinput)))
    (is (=  nil (sol1 input)))
    (is (=  nil (sol2 input)))))
