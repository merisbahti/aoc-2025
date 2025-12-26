(ns aoc-2025.day10
  (:require
   [aoc-2025.core :refer [get-input-for-day]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [clojure.math.combinatorics :as combo])
  (:import [com.microsoft.z3 Context Solver Status BoolExpr ArithExpr]))

(def testinput "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")
(def input (get-input-for-day))

(defn press-button [machine button]
  (assert (associative? machine))
  (reduce  (fn [machine button-effect]

             (update machine button-effect
                     #(case % \# \. \. \#))) machine button))

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map #(re-seq #"\[([.#]+)\]|\(([\d,]+)\)|\{([\d,]+)\}" %))
       (map #(map (comp (partial some identity) (partial drop 1)) %))
       (map (fn [groups]
              (let [machine (seq (first groups))
                    buttons (->> groups (drop 1) (drop-last) (map #(map parse-long (str/split % #","))))
                    joltages (map parse-long (str/split (last groups) #","))]

                [machine buttons joltages])))))

(defn sol1 [input]
  (->>
   input
   parse-input
   (map
    (fn
      [[machine buttons]]
      (->> (map
            (fn [subset]
              [subset  (press-button (vec machine) (flatten subset))])
            (combo/subsets buttons))
           (filter (fn [[_subset machine]] (every? (partial = \.) machine)))
           (map first)
           (apply min-key count))))
   (map count)
   (apply +)))

(defn sol2 [input]
  (->>
   (parse-input input)
   (map
    (fn [[_ buttons joltages]]
      (let
       [ctx (Context.)
        opt (.mkOptimize ctx)
        eq (fn [a b] (.mkEq ctx a b))
        ge (fn [a b] (.mkGe ctx a b))
        add (fn [& args] (.mkAdd ctx (into-array ArithExpr args)))
        int-val #(.mkInt ctx %)
        button-variables
        (mapv #(.mkIntConst ctx (str "x" %)) (range (count buttons)))]
        (doseq [x button-variables]
          (.Add opt (into-array BoolExpr [(ge x (int-val 0))])))
        (doseq
         [[jolt-i joltage] (map-indexed (fn [& rest] rest) joltages)]
          (.Add opt
                (into-array BoolExpr
                            [(eq (apply add
                                        (keep-indexed
                                         (fn [button-i button-joltages]
                                           (when (some (partial = jolt-i) button-joltages)
                                             (button-variables button-i)))
                                         buttons))
                                 (int-val joltage))])))
        (.MkMinimize opt (apply add button-variables))
        (when (= (.Check opt (into-array BoolExpr [])) Status/SATISFIABLE)
          (let [model (.getModel opt)]
            {:values (mapv #(-> (.eval model % false) .toString Integer/parseInt) button-variables)
             :sum (reduce + (map #(-> (.eval model % false) .toString Integer/parseInt) button-variables))})))))
   (map :sum)
   ((partial apply +))))

(deftest input-tests
  (testing "part 1"
    (is (= 7 (sol1 testinput)))
    (is (= 404 (sol1 input)))
    (is (= 33 (sol2 testinput)))
    (is (= 16474 (sol2 input)))))
