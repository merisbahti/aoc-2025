(ns aoc-2025.day6
  (:require
   [aoc-2025.core :refer [get-input-for-day]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def testinput
  "123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +  ")

(def input (get-input-for-day))

(defn sol1 [input] (->>
                    (str/split input #"\n")
                    (map str/trim)
                    (map #(str/split % #"\s+"))
                    (map (fn [items]
                           (map (fn [value]
                                  (cond
                                    (= value "*") *
                                    (= value "+") +
                                    :else (parse-long value))) items)))
                    (reduce (fn [acc curr]
                              (map cons curr acc))
                            (repeat []))

                    (map eval)
                    (apply +)))

(defn get-col-lengths [input]
  (->
   (str/split input #"\n")
   (last)
   (str/split #"")
   (#(loop [curr-nrs []
            remaining %]
       (let [head-char (str (first remaining))
             rest-char (apply str (rest remaining))
             [head-acc & rest-acc :as acc] curr-nrs]

         (cond
           (= remaining "")      (reverse curr-nrs)
           (not= head-char " ")  (recur (cons head-char acc) rest-char)
           (= head-char " ")     (recur (cons (str "x" head-acc) rest-acc) rest-char)
           :else (assert false "Unhandled case")))))
   (#(let [rows %]
       (map-indexed (fn [i col] (- (count col) (if (= (+ 1 i) (count rows)) 0 1))) rows)))
   (#(take 10000 %))))
(assert (= (get-col-lengths testinput) '(3 3 3 3)))
(assert (= (get-col-lengths "+ +  +  ") '(1 2 3)))
(assert (= (take 7 (get-col-lengths input)) '(3 4 3 4 2 2 2)))

(->>
 (str/split testinput #"\n")
 (map str/trim)
 (map #(str/split % #"\s+")))

(defn sol2 [input] nil)

(deftest input-tests
  (testing "part 1"
    (is (= 4277556 (sol1 testinput)))
    (is (= 4951502530386 (sol1 input)))
    (is (= nil (sol2 testinput)))
    (is (= nil (sol2 input)))))
