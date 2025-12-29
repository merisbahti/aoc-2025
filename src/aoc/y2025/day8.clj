(ns aoc.y2025.day8
  (:require
   [aoc.core :refer [get-input-for-day]]
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

(defn connect-circuits [p1 p2 circuits]
  (loop [[head & tail :as remaining] circuits
         p1-circuit nil
         p2-circuit nil
         acc []]
    (cond (and p1-circuit p2-circuit) (let [new-circuit (into p1-circuit p2-circuit)]
                                        (concat acc (list new-circuit) remaining))
          (empty? remaining) (assert false (str "Could not find circuits to connect: " (seq p1) " and " (seq p2) " with "))
          (and (head p1) (head p2))
          circuits
          (head p1) (recur tail head p2-circuit acc)
          (head p2) (recur tail p1-circuit head acc)
          :else (recur tail p1-circuit p2-circuit (cons head acc)))))

(deftest add-to-circuits-test
  (testing "part 1"
    (is (= [#{1 2}]					 (connect-circuits 1 2 [#{2} #{1}])))
    (is (= [#{1 2 3 5 7 8}]					 (connect-circuits 1 2 [#{2 7 8} #{1 3 5}])))))

(assert (= (dist [1 1 0] [2 2 0]) (math/sqrt 2)))
(assert (= (dist [1 1 1] [2 2 2]) (math/sqrt 3)))
(assert (= (dist [1 2 3] [2 3 4]) (math/sqrt 3)))
(assert (= (dist [0 0 0] [0 0 0]) 0.0))

(defn sol1 [input]
  (->>
   (str/split input #"\n")
   (map #(map parse-long (str/split %  #",")))
   ((fn [points]
      (let [point-distances
            (->>
             (combo/combinations points 2)
             (map (fn [[p1 p2]] [(dist p1 p2) p1 p2]))
             (sort-by first))
            max-iters (case (count points)
                        20 10
                        1000 1000)]

        (loop [iters 0
               circuits (map (fn [point] (set [point])) points)
               [[_ min-p1 min-p2] & remaining-point-distances] point-distances]

          (if (>= iters max-iters)
            circuits
            (let [new-circuits
                  (connect-circuits min-p1 min-p2 circuits)]

              (recur (+ 1 iters)
                     new-circuits
                     remaining-point-distances)))))))
   (map count)
   (sort-by -)
   (take 3)
   (apply *)))

(defn sol2 [input]
  (->>
   (str/split input #"\n")
   (map #(map parse-long (str/split %  #",")))
   ((fn [points]
      (let [point-distances
            (->>
             (combo/combinations points 2)
             (map (fn [[p1 p2]] [(dist p1 p2) p1 p2]))
             (sort-by first))]

        (loop [iters 0
               circuits (map (fn [point] (set [point])) points)
               [[_ min-p1 min-p2] & remaining-point-distances] point-distances]

          (let [new-circuits
                (connect-circuits min-p1 min-p2 circuits)]

            (if (= 1 (count new-circuits))
              (* (first min-p1) (first min-p2))
              (recur (+ 1 iters)
                     new-circuits
                     remaining-point-distances)))))))))

(deftest input-tests
  (testing "part 1"
    (is (= 40 (sol1 testinput)))
    (is (= 122430 (sol1 input)))
    (is (= 25272 (sol2 testinput)))
    (is (= 8135565324 (sol2 input)))))
