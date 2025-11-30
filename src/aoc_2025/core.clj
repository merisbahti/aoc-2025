(ns aoc-2025.core
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(defmacro get-input-for-day []
  "gets input for the day based on the filename"
  (let [filename (->
                  *file*
                  (str/split  #"/")
                  (last)
                  (str/split  #"\.")
                  (first)
                  (str ".txt"))]
    (if-let  [file (io/resource filename)]
      (slurp file)
      (throw (Exception. (str "File not found, looked at: " filename))))))
