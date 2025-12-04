(ns aoc-2025.demoday12024
  (:require
   [clojure.string :as str]
   [clojure.test :refer [is testing deftest]]
   [aoc-2025.core :refer [get-input-for-day]]))

(def demoinput "3   4
4   3
2   5
1   3
3   9
3   3")

(def input (get-input-for-day))

(defn sol1 [input]
  (->> input
       (#(str/split % #"\n"))
       (map (fn [row] (str/split row #"\s+")))
       (reduce
        (fn [[first-arr snd-arr] [fst snd]] [(into first-arr [(parse-long fst)]) (into  snd-arr [(parse-long snd)])])
        [[] []])
       (map sort)
       (apply map (fn [a b] (abs (- a b))))
       (apply +)))

(defn sol2 [input]
  (->> input
       (#(str/split % #"\n"))
       (map (fn [row] (str/split row #"\s+")))
       (reduce
        (fn [[first-arr snd-arr] [fst snd]] [(into first-arr [(parse-long fst)]) (into  snd-arr [(parse-long snd)])])
        [[] []])
       (map sort)
       (#(let [numbers (first %) freq (frequencies (second %))]
           (map (fn [x] (* x (get  freq x 0))) numbers)))
       (apply +)))

(frequencies [1 1 2 2 33 3])
(apply + '(0 0 9 9 9 4))

(deftest input-tests
  (testing "part 1"
    (is (= (sol1 demoinput) 11))
    (is (= (sol1 input) 1882714))
    (is (= (sol2 demoinput) 31))
    (is (= (sol2 input) 19437052))))
