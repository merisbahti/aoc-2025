(ns aoc.y2015.day11
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

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

(defn incr-base-64-str [s]
  (if (every? #(= % \z) s)
    (str/join (repeat (inc (count s)) \a))
    (loop [chars (reverse (vec s))
           acc []
           carry 1]
      (if (or (empty? chars) (= carry 0))
        (str/join (concat (reverse chars) (reverse acc)))
        (let [[curr-char & rest-chars] chars]
          (if (= curr-char \z)
            (recur rest-chars (conj acc \a) 1)

            (recur rest-chars (conj acc (char (+ 1 (int curr-char)))) 0)))))))

(defn sol1 [input]
  (->> (iterate incr-base-64-str input)
       (drop 1)
       (filter (fn [curr-str]
                 (and (has-increasing-chars? curr-str)
                      (has-two-repeated-nrs? curr-str)
                      (doesnt-contain-iol? curr-str))))
       (first)))

(deftest input-tests
  (testing "part 1"
    (is (= "vzbxxyzz" (sol1 "vzbxkghb")))
    (is (= "vzcaabcc" (sol1 "vzbxxyzz")))))
