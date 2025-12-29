(ns aoc.y2025.day9
  (:require
   [aoc.core :refer [get-input-for-day]]
   [clojure.string :as str]
   [clojure.math :as math]
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
  (let [minX (apply min (map first points))
        minY (apply min (map second points))
        maxX (apply max (map first points))
        maxY (apply max (map second points))
        point-set (set points)]
    (doseq [y (range minY (inc maxY))]
      (doseq [x (range minX (inc maxX))]
        (if (point-set [x y])
          (print "#")
          (print ".")))
      (println))))

(defn rasterize [points]
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

(defn flood-fill [grid]
  (let [directions [[0 1] [1 0] [0 -1] [-1 0]]
        xs (map first grid)
        ys (map second grid)
        minX (- (apply min xs) 1)
        maxX (+ (apply max xs) 1)
        minY (- (apply min ys) 1)
        maxY (+ (apply max ys) 1)
        outside-bounds (fn [[x y]] (or (< x minX) (> x maxX) (< y minY) (> y maxY)))
        grid-set (into #{} grid)]

    (loop [stack   [[minX minY]]
           visited #{}]
      (if (empty? stack)
        (filter (complement visited) (for [x (range minX maxX) y (range minY maxY)] [x y]))
        (let [[x y]       (first stack)
              candidates  (map (fn [[dX dY]] [(+ x dX) (+ y dY)]) directions)
              not-visited (filterv (fn [point]
                                     (not (or
                                           (grid-set point)
                                           (outside-bounds point)
                                           (visited point))))
                                   candidates)]
          (recur
           (into (rest stack) not-visited)
           (conj visited [x y])))))))

(defn sol2 [input]
  (->> input
       (str/split-lines)
       (map #(str/split % #","))
       (map (juxt (comp parse-long first) (comp parse-long second)))
       (into [])
       ((fn [points]
          (let [uniqX (->> points (map first) (set) (sort) (map-indexed (fn [i x] [x i])))
                uniqY (->> points (map second) (set) (sort) (map-indexed (fn [i x] [x i])))
                xMap (into {} uniqX)
                yMap (into {} uniqY)
                initialGrid (into [] (map (fn [[x y]] [(xMap x) (yMap y)]) points))

                allowedGrid (->> initialGrid
                                 (rasterize)
                                 (flood-fill)
                                 (into #{}))
                candidateAreas (->>
                                points
                                (#(combo/combinations % 2))
                                (filter (fn [[[x1 y1] [x2 y2]]]
                                          (let [p1 [(xMap x1) (yMap y1)]
                                                p2 [(xMap x2) (yMap y2)]
                                                p3 [(first p1) (second p2)]
                                                p4 [(first p2) (second p1)]
                                                shape-grid (rasterize [p1 p3 p2 p4])]

                                            (every? allowedGrid shape-grid))))

                                (map
                                 (fn [[[x1 y1] [x2 y2]]]
                                   (*
                                    (+ 1 (abs (- x1 x2)))
                                    (+ 1 (abs (- y1 y2)))))))]

            (apply max candidateAreas))))))

(deftest input-tests
  (testing "part 1"
    (is (= 50 (sol1 testinput)))
    (is (= 4748826374 (sol1 input)))
    (is (= 24 (sol2 testinput)))
    (is (= 1554370486 (sol2 input)))))
