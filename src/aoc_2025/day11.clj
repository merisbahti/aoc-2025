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

(def find-paths
  (memoize
   (fn [from to graph]
     (if (= from to)
       [[to]]
       (mapcat
        (fn [option]
          (let [paths (find-paths option to graph)]
            (when (seq paths) (map (fn [path] (cons from path)) paths))))
        (graph from))))))

(defn sol1 [input]
  (->> (str/split-lines input)
       (map (fn [line]
              (let [[key & valuestring] (str/split line #": ")
                    values (str/split (first valuestring) #" ")]
                [(keyword key) (map keyword values)])))
       (into {})
       (find-paths :you :out)
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

(defn sol2 [input]
  (->> (str/split-lines input)
       (map (fn [line]
              (let [[key & valuestring] (str/split line #": ")
                    values (str/split (first valuestring) #" ")]
                [(keyword key) (map keyword values)])))
       (into {})

       ((fn [graph]
          (let [svr-fft (find-paths :svr :fft graph)
                fft-dac (find-paths :fft :dac graph)
                dac-out (find-paths :dac :out graph)]

            (*
             (count svr-fft)
             (count fft-dac)
             (count dac-out)))))))

(deftest input-tests
  (testing "part 1"
    (is (= 5 (sol1 testinput)))
    (is (= 413 (sol1 input)))
    (is (= 2 (sol2 test-input-2)))
    (is (= 525518050323600 (sol2 input)))))
