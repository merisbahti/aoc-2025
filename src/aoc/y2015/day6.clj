(ns aoc.y2015.day6
  (:require
   [aoc.core :refer [get-input-for-day]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def testinput "")
(def input (get-input-for-day))

(defn sol1 [input]
  (->>
   (str/split-lines input)
   (reduce (fn [state line]
             (let [[instruction-str from-str to-str]
                   (re-seq #"turn on|turn off|toggle|[0-9]+,[0-9]+" line)
                   [fromX fromY] (mapv parse-long (str/split from-str #","))
                   [toX toY] (mapv parse-long (str/split to-str #","))
                   instruction (case instruction-str
                                 "turn on" (constantly true)
                                 "turn off" (constantly false)
                                 "toggle" (partial not))]

               (reduce
                (fn [acc [x y]]
                  (update-in acc [y x] instruction))
                state
                (for [x (range fromX (inc toX))
                      y (range fromY (inc toY))]
                  [x, y]))))

           (->> (range 0 (inc 1000))
                (mapv
                 (fn [_] (->>
                          (range 0 (inc 1000))
                          (mapv (constantly false)))))))

   (flatten)
   (filter true?)
   (count)))

(update [0] 0 (partial  + 2))

(defn sol2 [input]
  (->>
   (str/split-lines input)
   (reduce (fn [state line]
             (let [[instruction-str from-str to-str]
                   (re-seq #"turn on|turn off|toggle|[0-9]+,[0-9]+" line)
                   [fromX fromY] (mapv parse-long (str/split from-str #","))
                   [toX toY] (mapv parse-long (str/split to-str #","))
                   instruction (case instruction-str
                                 "turn on" (partial + 1)
                                 "turn off" (fn [x] (max 0 (- x 1)))
                                 "toggle" (partial + 2))]

               (reduce
                (fn [acc [x y]]
                  (update-in acc [y x] instruction))
                state
                (for [x (range fromX (inc toX))
                      y (range fromY (inc toY))]
                  [x, y]))))

           (->> (range 0 (inc 1000))
                (mapv
                 (fn [_] (->>
                          (range 0 (inc 1000))
                          (mapv (constantly 0)))))))

   (flatten)
   ((partial apply +))))

(deftest input-tests
  (testing "part 1"
    (is (= 543903 (sol1 input)))
    (is (= nil (sol2 testinput)))
    (is (= nil (sol2 input)))))
