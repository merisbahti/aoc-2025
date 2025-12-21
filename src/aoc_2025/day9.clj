(ns aoc-2025.day9
  (:require
   [aoc-2025.core :refer [get-input-for-day]]
   [clojure.string :as str]

   [clojure.test :refer [deftest is testing]]
   [clojure.math.combinatorics :as combo]))

(def testinput "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3")

(def input (get-input-for-day))

(defn sol1 [input] (->> input
                        (str/split-lines)
                        (map #(str/split % #","))
                        (map (fn [[x y]] [(parse-long x) (parse-long y)]))
                        (#(combo/combinations % 2))
                        (map (fn [[[x1 y1] [x2 y2]]]
                               (*
                                (+ 1 (abs (- x1 x2)))
                                (+ 1 (abs (- y1 y2))))))
                        (apply max)))

(defn draw-input [ranges]
  (let [min-y (-  (apply min (map first ranges)) 1)
        max-y (+ 1 (apply max (map first ranges)))
        xs (set (mapcat second ranges))
        min-x (-  (apply min xs) 2)
        max-x (+ 2 (apply max xs))
        points-set (set (mapcat (fn [[y [min-x max-x]]]
                                  (map (fn [x] [x y])
                                       (range min-x (inc max-x)))) ranges))]
    (force (map println ranges))
    (println "ranges" ranges)
    (println xs min-x max-x min-y max-y points-set)
    (doseq [y (range min-y (+ 1 max-y))]
      (doseq [x (range min-x (+ 1 max-x))]
        (if (points-set [x y])
          (print "#")
          (print ".")))
      (println))))

(->> testinput
     (str/split-lines)
     (map #(str/split % #","))
     (map (fn [[x y]] [(parse-long x) (parse-long y)]))
     ((fn [points]
        (let [sorted-by-ys (->> (sort-by (juxt second first) points) (into []))
              x-ranges (->> (range 0 (+  (/ (count points) 2)))
                            (map
                             (fn [i]
                               (let [[low-x low-y] (get sorted-by-ys (* 2 i))
                                     [high-x high-y] (get sorted-by-ys (inc (* 2 i)))]
                                 (assert (= low-y high-y))
                                 [low-y [low-x high-x]]))))]

          x-ranges)))
     (reduce
      (fn [{[prev-y [prev-min-x prev-max-x] :as prev-range] :prev-range ranges :ranges}
           [curr-y [curr-min-x curr-max-x] :as curr-range]]
        (let [new-range
              (if prev-range
                (cond
                  (= prev-min-x curr-max-x) [curr-y [curr-min-x prev-max-x]]
                  (= prev-min-x curr-min-x) [curr-y [curr-max-x prev-max-x]]

                  :else (assert false))
                curr-range)
              added-ranges  (when prev-range (map (fn [y]
                                                    [y [prev-min-x prev-max-x]]) (range prev-y (inc curr-y))))]

          {:prev-range new-range
           :ranges (concat (reverse added-ranges) ranges)}))
      {:prev-range nil :ranges []})
     (:ranges)
     (reverse)
     (draw-input)

;; (mapcat
     ;;  (fn [[k [from to]]]
     ;;    (map (fn [x] [x k])
     ;;         (range from (+ 1 to)))))
     ;; (sort-by second)
     ;; (draw-input)
     )
(defn sol2 [input] nil)

(deftest input-tests
  (testing "part 1"
    (is (= 50 (sol1 testinput)))
    (is (= 4748826374 (sol1 input)))
    (is (= nil (sol2 testinput)))
    (is (= nil (sol2 input)))))
