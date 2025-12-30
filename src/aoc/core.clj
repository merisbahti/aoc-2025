(ns aoc.core
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(defmacro get-input-for-day []
  "gets input for the day based on the filename and year from caller's namespace"
  (let [ns-name (str *ns*)
        year (second (re-find #"y(\d+)" ns-name))
        filename (->
                  *file*
                  (str/split  #"/")
                  (last)
                  (str/split  #"\.")
                  (first)
                  (str ".txt"))
        path (if year
               (str "y" year "/" filename)
               filename)]
    `(if-let [file# (io/resource ~path)]
       (slurp file#)
       (let [resources-path# (io/file "resources" ~path)
             dir# (.getParentFile resources-path#)]
         (when-not (.exists dir#)
           (.mkdirs dir#))
         (when-not (.exists resources-path#)
           (spit resources-path# ""))
         (slurp resources-path#)))))
