(ns aoc-2025.day8
  (:require
   [aoc-2025.core :refer [get-input-for-day]]
   [clojure.math :as math]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [clojure.spec.alpha :as s]
   [clojure.math.combinatorics :as combo]))

(def testinput "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689")
(def input (get-input-for-day))

(defn dist [point1 point2]
  (assert (= (count point1) (count point2)))
  (->>
   (map (fn [x y] (- x y)) point1 point2)
   (map (fn [x] (math/pow x 2)))
   (apply +)
   (math/sqrt)))

(time
 (->>
  (str/split testinput #"\n")
  (map #(map parse-long (str/split %  #",")))
  ((fn [points]
     (let [point-distances
           (->>
            (combo/combinations points 2)
            (map (fn [[p1 p2]] [(dist p1 p2) p1 p2])))]
       point-distances
       ;; (loop [circuits []]
       ;;   (let [flat-circuits (reduce cons [] circuits)]

       ;;     (if (= (count points) (count flat-circuits))
       ;;       circuits
       ;;       (recur [points]))))
       )))))

(assert (= (dist [1 1 0] [2 2 0]) (math/sqrt 2)))
(assert (= (dist [1 1 1] [2 2 2]) (math/sqrt 3)))
(assert (= (dist [1 2 3] [2 3 4]) (math/sqrt 3)))
(assert (= (dist [0 0 0] [0 0 0]) 0.0))

(defn sol1 [input] nil)
(defn sol2 [input] nil)

(mapcat identity (chunked [1 2 3] 1))

(deftest input-tests
  (testing "part 1"
    (is (= nil (sol1 testinput)))
    (is (= nil (sol1 input)))
    (is (= nil (sol2 testinput)))
    (is (= nil (sol2 input)))))
