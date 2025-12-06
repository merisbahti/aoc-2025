(ns aoc-2025.day1
  (:require
   [clojure.math :as math]
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

(defn count-zeros [start diff]
  (let [factor (if (neg? diff) -1 1)]
    (loop [curr start
           remaining (abs diff)
           zeros 0]
      (let* [new-curr (mod (+ curr factor) 100)]
            (if (= remaining 0) zeros
                (recur new-curr
                       (- remaining 1)
                       (+ zeros (if (= new-curr 0) 1 0))))))))

(defn sol1 [input] (->> (str/split input #"\n")
                        (map
                         (fn [row]
                           (let [[_ polarity count] (re-find (re-matcher #"([LR])(\d+)" row))]
                             (* (if (= polarity "R") 1 -1) (parse-long count)))))

                        (reduce (fn [[head & tail] curr]
                                  (cons (mod (+ curr head) 100) (cons head tail))) [50])
                        (filter (partial = 0)) (count)))

(defn sol2 [input] (->> (str/split input #"\n")
                        (map
                         (fn [row]
                           (let [[_ polarity count] (re-find (re-matcher #"([LR])(\d+)" row))]
                             (* (if (= polarity "R") 1 -1) (parse-long count)))))

                        (reduce
                         (fn [{latest :latest zeros :zeros} curr]
                           (let* [sum (+  latest curr)
                                  new-latest (mod sum 100)

                                  new-zeros (+ zeros  (count-zeros latest curr))]
                                 {:latest new-latest
                                  :zeros new-zeros}))

                         {:latest 50 :zeros 0})
                        (:zeros)))

(mod -10 100)

(deftest input-tests
  (testing "part 1"
    (is (= 3 (sol1 testinput)))
    (is (= 1105 (sol1 input)))
    (is (= 6 (sol2 testinput)))
    (is (= 6599 (sol2 input)))))
(deftest count-zero-test
  (testing "stuff"
    (is (= 1 (count-zeros 50 -83)))
    (is (= 10 (count-zeros 50 1049)))
    (is (= 11 (count-zeros 50 1050)))
    (is (= 0 (count-zeros 50 49)))
    (is (= 0 (count-zeros 98 1)))
    (is (= 0 (count-zeros 99 0)))
    (is (= 0 (count-zeros 82 -30)))
    (is (= 0 (count-zeros 0 -1)))
    (is (= 0 (count-zeros 0 1)))
    (is (= 1 (count-zeros 1 -1)))
    (is (= 1 (count-zeros 99 1)))
    (is (= 1 (count-zeros 95 60)))))
