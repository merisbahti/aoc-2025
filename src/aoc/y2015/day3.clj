(ns aoc.y2015.day3
  (:require
   [aoc.core :refer [get-input-for-day]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def testinput1 "^v")
(def testinput2 "^>v<")
(def testinput3 "^v^v^v^v^v")
(def input (get-input-for-day))

(defn sol1 [input]
  (->> (str/split input #"")
       (reduce (fn [[[x y] visited] move]
                 (let [next-pos
                       (case move
                         "^" [x (+ 1 y)]
                         "v" [x (+ -1 y)]
                         "<" [(+ -1 x) y]
                         ">" [(+ 1 x) y])]
                   [next-pos (into visited [next-pos])]))
               [[0 0] #{}])

       (second)
       (count)))
(defn calc-pos [[x y] move]
  (case move
    "^" [x (+ 1 y)]
    "v" [x (+ -1 y)]
    "<" [(+ -1 x) y]
    ">" [(+ 1 x) y]))
(defn sol2 [input]
  (->> (str/split input #"")
       (reduce (fn [[apos bpos aturn visited] move]
                 (let [next-a
                       (if aturn
                         (calc-pos apos move)
                         apos)
                       next-b
                       (if-not aturn (calc-pos bpos move)
                               bpos)
                       next-aturn (not aturn)]

                   [next-a next-b next-aturn (into visited [next-a next-b])]))
               [[0 0] [0 0] true #{}])

       (drop 3)
       (first)
       (count)))

(deftest input-tests
  (testing "part 1"
    (is (= 2 (sol1 testinput1)))
    (is (= 4 (sol1 testinput2)))
    (is (= 2 (sol1 testinput3)))
    (is (= nil (sol1 input)))
    (is (= 3 (sol2 testinput1)))
    (is (= 3 (sol2 testinput2)))
    (is (= 11 (sol2 testinput3)))
    (is (= nil (sol2 input)))))
