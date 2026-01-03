(ns aoc.y2015.day5
  (:require
   [aoc.core :refer [get-input-for-day]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def testinput "")
(def input (get-input-for-day))

(defn is-nice? [s]
  (and
   (seq? (re-seq #"(\w)\1" s))
   (>= (count (re-seq #"[aeiou]" s)) 3)
   (= (count (re-seq #"ab|cd|pq|xy" s)) 0)))

(defn sol1 [input]
  (->> (str/split-lines input)
       (filter is-nice?)
       (count)))

(defn is-nice-2? [s]
  (and
   (>= (count (re-seq #"(\w\w).*\1" s)) 1)
   (>= (count (re-seq #"(\w)\w\1" s)) 1)))

(defn sol2 [input]
  (->> (str/split-lines input)
       (filter is-nice-2?)
       (count)))

(deftest input-tests
  (testing "part 1"
    (is (= true (is-nice? "ugknbfddgicrmopn")))
    (is (= true (is-nice? "aaa")))
    (is (= false (is-nice? "jchzalrnumimnmhp")))
    (is (= false (is-nice? "haegwjzuvuyypxyu")))
    (is (= false (is-nice? "dvszwmarrgswjxmb")))
    (is (= 255 (sol1 input)))

    (is (= true (is-nice-2? "qjhvhtzxzqqjkmpb")))
    (is (= true (is-nice-2? "xxyxx")))
    (is (= false (is-nice-2? "uurcxstgmygtbstg")))
    (is (= false (is-nice-2? "ieodomkazucvgmuy")))
    (is (= 255 (sol2 input)))))
