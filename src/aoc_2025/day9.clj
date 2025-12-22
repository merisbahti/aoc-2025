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

(defn draw-grid [points]
  (let [maxX (apply max (map first points))
        maxY (apply max (map second points))]
    (doseq [y (range (inc maxY))]
      (doseq [x (range (inc maxX))]
        (if (some #(and (= x (first %)) (= y (second %))) points)
          (print "#")
          (print ".")))
      (println))))

(let [arr [1 2 3]]
  (arr (mod 3 (count arr))))
(defn rasterize [points]
  ;; is clojure array
  (assert (vector? points))
  (->> points
       (map-indexed (fn [& rest] rest))
       (mapcat
        (fn [[index pointA]]
          (let [[fromX fromY] pointA
                [toX toY] (points (mod (+ 1 index) (count points)))]
            (cond
              (= fromX toX) (map (fn [y] [fromX y])
                                 (let [vs [fromY toY]
                                       minV (apply min vs)
                                       maxV (apply max vs)]

                                   (range minV (+ 1 maxV))))

              (= fromY toY) (map (fn [x] [x fromY])
                                 (let [vs [fromX toX]
                                       minV (apply min vs)
                                       maxV (apply max vs)]

                                   (range minV (+ 1 maxV))))
              :else (throw (Exception. "Diagonal line not supported"))))))))
(range -2 1)
(->> testinput
     (str/split-lines)
     (map #(str/split % #","))
     (map (juxt (comp parse-long first) (comp parse-long second)))
     (into [])
     ((fn [points]
        (let [uniqX (->> points (map first) (set) (sort) (map-indexed (fn [i x] [x i])))
              uniqY (->> points (map second) (set) (sort) (map-indexed (fn [i x] [x i])))
              xMap (into {} uniqX)
              yMap (into {} uniqY)
              initialGrid (into [] (map (fn [[x y]] [(xMap x) (yMap y)]) points))]
          (println "hi")
          (draw-grid initialGrid)
          (println "hi")
          (draw-grid (rasterize initialGrid))))))

(defn sol2 [input] nil)

(deftest input-tests
  (testing "part 1"
    (is (= 50 (sol1 testinput)))
    (is (= 4748826374 (sol1 input)))
    (is (= nil (sol2 testinput)))
    (is (= nil (sol2 input)))))
