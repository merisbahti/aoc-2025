(ns aoc.y2015.day11
  (:require
   [aoc.core :refer [get-input-for-day]]
   [clojure.string :as str]
   [clojure.math :as math]
   [clojure.test :refer [deftest is testing]]))

(defn nr-to-chars [n]
  (let [char-count (math/ceil (/ (math/log n) (math/log 26)))]
    (->> (loop [remaining-amount n
                curr-number char-count
                acc []]

           (if (zero? curr-number)
             acc
             (let [pos-value (math/floor (/ remaining-amount (math/pow 26 (dec curr-number))))
                   new-remaining-amount (- remaining-amount (* pos-value (math/pow 26 (dec curr-number))))]
               (recur new-remaining-amount
                      (dec curr-number)
                      (conj acc (char (+ 97  pos-value)))))))
         (str/join))))

(defn str-to-base-26 [s]
  (->> (vec s)
       (reverse)
       (map-indexed
        (fn [i c]
          (* (- (int c) 97) (math/pow 26 i))))
       ((partial apply +))))
(def increasing-chars
  (map (fn [c]
         (str/join [(char c) (char (+ 1 c)) (char (+ 2 c))]))

       (range (int \a) (inc (int \x)))))

(defn has-increasing-chars? [mystr]
  (re-find #"(abc|bcd|cde|def|efg|fgh|ghi|hij|ijk|jkl|klm|lmn|mno|nop|opq|pqr|qrs|rst|stu|tuv|uvw|vwx|wxy|xyz)" mystr))

(defn has-two-repeated-nrs? [mystr]
  (->> mystr
       (re-seq #"(\w)\1")
       (map first)
       (into #{})
       (count)
       (#(>= % 2))))

(defn doesnt-contain-iol? [str]
  (not (re-find #"[iol]" str)))

(defn sol1 [input]
  (let [base-nr (str-to-base-26 input)]
    (->> (range 1 1000000) (map (fn [x] (nr-to-chars (+ x base-nr))))
         (filter (fn [curr-str]
                   (and (has-increasing-chars? curr-str)
                        (has-two-repeated-nrs? curr-str)
                        (doesnt-contain-iol? curr-str))))
         (first))))

(deftest input-tests
  (testing "part 1"
    (is (= "vzbxxyzz" (sol1 "vzbxkghb")))
    (is (= "vzcaabcc" (sol1 "vzbxxyzz")))))
