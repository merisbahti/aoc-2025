(ns aoc.y2025.day5
  (:require
   [aoc.core :refer [get-input-for-day]]
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

"3-5
5-6
10-14
12-18
16-20"

(defn sol2 [input]
  (->>
   (-> (str/split input #"\n\n")
       first
       (str/split  #"\n"))
   (map #(let
          [matcher (re-matcher #"(?<start>\d+)-(?<end>\d+)" %)]
           (if (.matches matcher)
             {:start (parse-long (.group matcher 1))
              :end (parse-long (.group matcher 2))}
             (throw (.new Exception "oops")))))
   (sort-by :start)
   (reduce
    (fn
      [[{head-start :start head-end :end :as head} & tail :as acc]
       {curr-start :start curr-end :end :as curr}]
      (cond
        (nil? head-start) (cons curr acc)
        (and
         (<= head-start curr-start)
         (>=  head-end curr-end))
        acc
        (= curr head) acc

        (>= head-end curr-start) (cons {:start head-start :end curr-end} tail)
        :else (cons curr acc)))

    [])
   (map #(->>
          %
          ((juxt (comp (partial + 1) :end) :start))
          (apply -)))
   (apply +)))
(- 321958501747837 321958501747817)
(deftest input-tests
  (testing "part 1"
    (is (= 3 (sol1 testinput)))
    (is (= 10 (sol2 "1-10\n2-5")))
    (is (= 10 (sol2 "2-5\n1-10")))
    (is (= 567 (sol1 input)))
    (is (= 14 (sol2 testinput)))
    (is (= 354149806372909 (sol2 input)))))
