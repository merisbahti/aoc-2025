(ns aoc-2025.template
  (:require
   [aoc-2025.core :refer [get-input-for-day]]
   [clojure.string :as str]

   [clojure.test :refer [deftest is testing]]
   [clojure.math.combinatorics :as combo]))

(def testinput "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3")

(def input (get-input-for-day))

(defn sol1 [input] (->> input
                        (str/split-lines)
                        (map #(str/split % #","))
                        (map (fn [[x y]] [(parse-long x) (parse-long y)]))
                        (#(combo/combinations % 2))
                        (map (fn [[[x1 y1] [x2 y2]]]
                               (*
                                (+ 1 (abs (- x1 x2)))
                                (+ 1 (abs (- y1 y2))))))
                        (apply max)))

(->> testinput
     (str/split-lines)
     (map #(str/split % #","))
     (map (fn [[x y]] [(parse-long x) (parse-long y)]))
     ((fn [points]
        (let [sorted-by-ys (->> (sort-by (juxt second first) points) (into []))
              x-ranges (->> (map
                             (fn [i]
                               (let [[low-x low-y] (get sorted-by-ys (* 2 i))
                                     [high-x high-y] (get sorted-by-ys (inc (* 2 i)))]
                                 (assert (= low-y high-y))
                                 [low-y [low-x high-x]]))

                             (range 0 (+  (/ (count points) 2))))
                            (into {}))]

          x-ranges))))

(defn sol2 [input] nil)

(deftest input-tests
  (testing "part 1"
    (is (= 50 (sol1 testinput)))
    (is (= 4748826374 (sol1 input)))
    (is (= nil (sol2 testinput)))
    (is (= nil (sol2 input)))))
