(ns aoc-2025.day11
  (:require
   [aoc-2025.core :refer [get-input-for-day]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def testinput "aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out")

(def input (get-input-for-day))

(def find-paths (fn [[head-path :as curr-path] graph]
                  (assert (not= nil head-path))
                  (let [paths (head-path graph)]
                    (if (nil? paths)
                      [(reverse curr-path)]
                      (mapcat
                       (fn [k]
                         (find-paths (cons k curr-path) graph))
                       paths)))))

(defn sol1 [input]
  (->> (str/split-lines input)
       (map (fn [line]
              (let [[key & valuestring] (str/split line #": ")
                    values (str/split (first valuestring) #" ")]
                [(keyword key) (map keyword values)])))
       (into {})
       (find-paths [:you])
       (count)))

(def test-input-2 "svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out
")
(def dacset #{:dac})
(def fftset #{:fft})
(defn filterfn [path] (and (some dacset  path) (some fftset path)))

(defn sol2 [input]
  (->> (str/split-lines input)
       (map (fn [line]
              (let [[key & valuestring] (str/split line #": ")
                    values (str/split (first valuestring) #" ")]
                [(keyword key) (map keyword values)])))
       (into {})
       (find-paths [:svr])
       (filter filterfn)
       (count)))

(deftest input-tests
  (testing "part 1"
    (is (= 5 (sol1 testinput)))
    (is (= 413 (sol1 input)))
    (is (= 2 (sol2 test-input-2)))
    ;; (is (= nil (sol2 input)))
    ))
