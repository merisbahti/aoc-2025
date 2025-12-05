(ns aoc-2025.day3
  (:require
   [aoc-2025.core :refer [get-input-for-day]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def testinput "987654321111111
811111111111119
234234234234278
818181911112111
")

(def input (get-input-for-day))

(defn highest-joltage [input-string]
  (let* [indexed (->>
                  (str/split input-string #"")
                  (map-indexed (fn [i x] {:number (parse-long x) :index i})))
         sorted-by-number (sort-by (comp - :number) indexed)]
        (loop [curr-list sorted-by-number]
          (assert (seq curr-list))

          (let* [curr-head (first curr-list)
                 remaining-list   (filter #(> (:index %) (:index curr-head)) sorted-by-number)
                 second-highest-number (first remaining-list)]

                (if second-highest-number
                  (parse-long (str (:number curr-head) (:number second-highest-number)))
                  (recur (rest curr-list)))))))

(deftest highest-joltage-tests
  (testing "highest-joltage"
    (is (=  99 (highest-joltage "991234")))
    (is (=  89 (highest-joltage "89")))
    (is (=  89 (highest-joltage "819")))
    (is (=  11 (highest-joltage "1111")))))

(defn highest-joltage-2 [input-string MAX_BANK_SIZE]
  (let*
   [indexed (->>
             (str/split input-string #"")
             (map-indexed (fn [i x] {:number (parse-long x) :index i})))
    sorted-by-number (sort-by (juxt (comp - :number) :index) indexed)]
   (loop [acc []]
     (let* [curr-last (last acc)
            remaining-items (- MAX_BANK_SIZE (count acc))
            candidate
            (first
             (filter
              #(and
                (not (some (partial = %) acc)) ;; is not in acc list
                (or (nil? curr-last) (> (:index %) (:index curr-last))) ;; is higher index than last
                (>= (- (count sorted-by-number) (:index %)) remaining-items) ;; has more items left
                )
              sorted-by-number))
            next-acc (conj  acc candidate)]
           (assert candidate)
           (if (>= (count next-acc) MAX_BANK_SIZE)
             (->> next-acc (map :number) (apply str) (parse-long))
             (recur  next-acc))))))
(conj [] 1)
(deftest highest-joltage-2-tests
  (testing "highest-joltage-2"
    (is (=  1 (highest-joltage-2 "12" 2)))
    (is (=  987654321111 (highest-joltage-2 "987654321111111" 12)))
    (is (=  811111111119 (highest-joltage-2 "811111111119" 12)))
    (is (=  434234234278 (highest-joltage-2 "234234234234278" 12)))
    (is (=  888911112111 (highest-joltage-2 "818181911112111" 12)))))

(defn sol1 [input]
  (->>
   (str/split input #"\n")
   (map highest-joltage)
   (reduce +)))
(defn sol2 [input]
  (->>
   (str/split input #"\n")
   (map #(highest-joltage-2 % 12))
   (reduce +)))

(deftest input-tests
  (testing "part 1"
    (is (= 357 (sol1 testinput)))
    (is (= 17427 (sol1 input)))
    (is (= 3121910778619 (sol2 testinput)))
    (is (= nil (sol2 input)))))
