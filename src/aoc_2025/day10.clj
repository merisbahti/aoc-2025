(ns aoc-2025.day10
  (:require
   [aoc-2025.core :refer [get-input-for-day]]
   [clojure.string :as str]
   [loco.core :as loco]
   [loco.constraints :as lc]
   [clojure.test :refer [deftest is testing]]
   [clojure.math.combinatorics :as combo])
  (:import [com.microsoft.z3 Context Solver Status BoolExpr ArithExpr]))

(defn add! [solver & constraints]
  (.add solver (into-array BoolExpr constraints)))
(for [[x y z] (vector [1 2 3] [1 2 3])]
  `(~x ~y ~z))
;; Then use:
(let [ctx (Context.)
      solver (.mkSolver ctx)
      xs (mapv #(.mkIntConst ctx (str "x" %)) (range 6))
      [x0 x1 x2 x3 x4 x5] xs
      opt (.mkOptimize ctx)
      eq (fn [a b] (.mkEq ctx a b))
      ge (fn [a b] (.mkGe ctx a b))
      add (fn [& args] (.mkAdd ctx (into-array ArithExpr args)))
      int-val #(.mkInt ctx %)]
  (doseq [x xs]
    (.Add opt (into-array BoolExpr [(ge x (int-val 0))])))

  (.Add opt (into-array BoolExpr [(eq (add x4 x5) (int-val 3))]))
  (.Add opt (into-array BoolExpr [(eq (add x1 x5) (int-val 5))]))
  (.Add opt (into-array BoolExpr [(eq (add x2 x3 x4) (int-val 4))]))
  (.Add opt (into-array BoolExpr [(eq (add x0 x1 x3) (int-val 7))]))

  (when (= (.Check opt (into-array BoolExpr [])) Status/SATISFIABLE)
    (let [model (.getModel opt)]
      {:values (mapv #(-> (.eval model % false) .toString Integer/parseInt) xs)
       :sum (reduce + (map #(-> (.eval model % false) .toString Integer/parseInt) xs))})))

(comment [(lc/$in [:x 0] 0 100)
          (lc/$in [:x 1] 0 100)
          (lc/$in [:x 2] 0 100)
          (lc/$in [:x 3] 0 100)
          (lc/$in [:x 4] 0 100)
          (lc/$in [:x 5] 0 100)
          (lc/$= (lc/$+ [:x 4] [:x 5]) 3)
          (lc/$= (lc/$+ [:x 1] [:x 5]) 5)
          (lc/$= (lc/$+ [:x 2] [:x 3] [:x 4]) 4)
          (lc/$= (lc/$+ [:x 0] [:x 1] [:x 3]) 7)
          (lc/$min (lc/$+ [:x 0] [:x 1] [:x 2] [:x 3] [:x 4] [:x 5]))])

(def testinput "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")
(def input (get-input-for-day))

(def pattern #"(?:\[(.*?)\]|\((.*?)\)|\{(.*?)\})")

(defn parse-groups [s]
  (let [matches (re-seq pattern s)]
    (map (fn [match]
           (let [;; A match for re-seq with capturing groups will be a vector:
                 ;; [full-match group1 group2 group3]
                 [_ sq-group parens-group braces-group] match]
             (cond
               (some? sq-group)    {:type :vector, :content sq-group}
               (some? parens-group) {:type :list, :content parens-group}
               (some? braces-group) {:type :set, :content braces-group}
               :else                 nil)))
         matches)))

(def parsed-data (parse-groups "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"))
(seq '("a" "b"))
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

;; (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
;; {0 0 0 0}
;; {0 0 0 1}
;; {0 2 0 3}
;; {0 2 4 7}
;; {3 5 4 7}
(def constraints
  [(lc/$in [:x 0] 0 100)
   (lc/$in [:x 1] 0 100)
   (lc/$in [:x 2] 0 100)
   (lc/$in [:x 3] 0 100)
   (lc/$in [:x 4] 0 100)
   (lc/$in [:x 5] 0 100)
   (lc/$= (lc/$+ [:x 4] [:x 5]) 3)
   (lc/$= (lc/$+ [:x 1] [:x 5]) 5)
   (lc/$= (lc/$+ [:x 2] [:x 3] [:x 4]) 4)
   (lc/$= (lc/$+ [:x 0] [:x 1] [:x 3]) 7)
   (lc/$min (lc/$+ [:x 0] [:x 1] [:x 2] [:x 3] [:x 4] [:x 5]))])
(loco/solution constraints)
;; (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
(->> (loco/solution
      [(lc/$in [:x 0] 0 100)
       (lc/$in [:x 1] 0 100)
       (lc/$in [:x 2] 0 100)
       (lc/$in [:x 3] 0 100)
       (lc/$in [:x 4] 0 100)

       (lc/$= (lc/$+ [:x 0] [:x 2] [:x 3]) 7)
       (lc/$= (lc/$+ [:x 3] [:x 4]) 5)
       (lc/$= (lc/$+ [:x 0] [:x 1] [:x 3] [:x 4]) 12)
       (lc/$= (lc/$+ [:x 0] [:x 1]  [:x 4]) 7)
       (lc/$= (lc/$+ [:x 0] [:x 2]  [:x 4]) 2)])

     (vals)
     ((partial apply +)))

(comment
  (->>
   (parse-input input)
   (map
    (fn [[_ buttons joltages]]
      (let
       [button-variables
        (map-indexed (fn [i _] [:x i]) buttons)
        joltage-button-thing
        (map-indexed
         (fn [jolt-i joltage]
           `(lc/$=
             (lc/$+
              ~@(filter (complement nil?)
                        (map-indexed
                         (fn [button-i button-joltages]
                           (when (some (partial = jolt-i) button-joltages)
                             [:x button-i]))
                         buttons)))
             ~joltage))
         joltages)

        button-declarations
        (map
         (fn [bv] `(lc/$in ~bv 0 500))
         button-variables)
        min-constraint [`(lc/$in :button-presses 0 1200)
                        `(lc/$= :button-presses (lc/$+ ~@button-variables))]]
        (concat button-declarations min-constraint
                joltage-button-thing))))

   (map #(map eval %))

   (pmap (fn [constraints] (loco/solution constraints :minimize :button-presses)))
   (map :button-presses)
   (vec)
   (filter nil?)
   (count)))

(defn sol2 [input] nil)

(deftest input-tests
  (testing "part 1"
    (is (= 7 (sol1 testinput)))
    (is (= 404 (sol1 input)))
    (is (= nil (sol2 testinput)))
    (is (= nil (sol2 input)))))
