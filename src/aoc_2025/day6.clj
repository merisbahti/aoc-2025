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
(parse-long (str/trim (str/replace "1  *" "*" "")))

(defn sol2 [input]
  (let [rows  (str/split input #"\n")
        column-indexes (range 0 (apply max (map count rows)))]

    (->>
     (reduce
      (fn [acc index]
        (let [string (str/trim (apply str (map #(get % index) rows)))]
          (cons string acc)))

      []  column-indexes)

     (reduce
      (fn [{sum :sum stack :stack :as acc} string]
        (println {:acc acc :string string})
        (cond
          (= "" (str/trim string))
          acc
          (str/ends-with? string "*")
          {:sum

           (+ sum
              (apply *
                     (cons
                      (parse-long (str/trim (str/replace string "*" ""))) stack)))
           :stack []}
          (str/ends-with? string "+") {:sum  (+ sum (apply + (cons (parse-long (str/trim (str/replace string "+" ""))) stack))) :stack []}
          :else {:sum sum :stack (cons (parse-long string) stack)}))

      {:sum 0 :stack []})
     (:sum))))

(deftest input-tests
  (testing "part 1"
    (is (= 4277556 (sol1 testinput)))
    (is (= 4951502530386 (sol1 input)))
    (is (= 3263827 (sol2 testinput)))
    (is (= 8486156119946 (sol2 input)))))
