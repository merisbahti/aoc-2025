(ns aoc.y2025.template
  (:require
   [aoc.core :refer [get-input-for-day]]
   [clojure.test :refer [deftest is testing]])
  (:import
   [java.security MessageDigest]))

(def testinput "abcdef")
(def input (get-input-for-day))

(defn leading-md5-zeros [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)]
    (- size (count sig))))

(defn sol1 [input]
  (->> (map (fn [val] [(leading-md5-zeros (str input val)) val]) (range))
       (filter (comp (partial = 5) first))
       (first)
       (second)))

(take 5 (range 10))
(defn sol2 [input startvalue]
  (->> (map (fn [val] [(leading-md5-zeros (str input val)) val]) (range startvalue))
       (filter (comp (partial = 6) first))
       (first)
       (second)))

(deftest input-tests
  (testing "part 1"
    (is (= 609043 (sol1 "abcdef")))
    (is (= 1048970 (sol1 "pqrstuv")))
    (is (= 346386 (sol1 "iwrupvqb")))

    (is (= 9958218 (sol2 "iwrupvqb")))))
