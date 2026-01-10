(ns aoc.y2015.day7
  (:require
   [aoc.core :refer [get-input-for-day]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [clojure.math :as math]))

(def testinput "123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
y -> o
NOT x -> h
NOT y -> i")

(defn flip-all [nr] (reduce (fn [acc curr] (bit-flip acc curr)) nr (range 0 16)))

(def input (get-input-for-day))

(defn sol1 [input to-eval]
  (let [eval-cache (atom {})
        exprs (->>
               (str/split-lines input)
               (mapv (fn [line]
                       (->> (str/split line #" -> ")
                            (reverse)
                            (into []))))
               (into {}))
        eval-expr (fn eval-expr [expr]
                    (let [cached (@eval-cache expr)]
                      (if
                       cached
                        cached
                        (let [evaled (condp re-matches expr
                                       #"(\w+)\s+AND\s+(\w+)" :>> (fn [[_ a b]] (bit-and (eval-expr a) (eval-expr b)))
                                       #"(\w+)\s+LSHIFT\s+(\w+)" :>> (fn [[_ a b]] (bit-shift-left (eval-expr a) (eval-expr b)))
                                       #"(\w+)\s+RSHIFT\s+(\w+)" :>> (fn [[_ a b]] (bit-shift-right (eval-expr a) (eval-expr b)))
                                       #"NOT\s+(\w+)" :>> (fn [[_ a]] (flip-all (eval-expr a)))
                                       #"(\w+)\s+OR\s+(\w+)" :>> (fn [[_ a b]] (bit-or (eval-expr a) (eval-expr b)))
                                       #"^(\d+)$" :>> (fn [[_ _]] (parse-long expr))
                                       #"^([a-zA-Z]+)$" :>> (fn [[_ _]] (eval-expr (exprs expr))))]
                          (swap! eval-cache (fn [map] (assoc map expr evaled)))
                          evaled))))]

    (eval-expr to-eval)))
(defn sol2 [input] nil)

(deftest input-tests
  (testing "part 1"
    (is (= 65412 (sol1 testinput "h")))
    (is (= 123 (sol1 testinput "x")))
    (is (= 456 (sol1 testinput "y")))
    (is (= 492 (sol1 testinput "f")))
    (is (= 114 (sol1 testinput "g")))
    (is (= 72 (sol1 testinput "d")))
    (is (= 456 (sol1 testinput "o")))
    (is (= 507 (sol1 testinput "e")))
    (is (= 956 (sol1 input "a")))
    (is (= nil (sol2 testinput)))
    (is (= nil (sol2 input)))))
