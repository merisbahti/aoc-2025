(defproject aoc-2025 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :profiles {:dev {:source-paths ["dev"]}}
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.3"]
                 [org.clojure/math.combinatorics "0.3.0"]
                 [org.clojure/spec.alpha "0.5.238"]
                 [metosin/malli "0.20.0"]
                 [loco "0.3.1"]
                 [tools.aqua/z3-turnkey "4.14.1"]]
  :test-paths ["src"]
  :repl-options {:init-ns aoc.core})
