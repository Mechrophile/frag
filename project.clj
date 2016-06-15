(defproject frag "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Unlicense"
            :url "http://unlicense.org/"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.36"]
                 [prismatic/plumbing "0.5.3"]
                 [prismatic/schema "1.1.2"]]
  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.2.11"]
                                  [criterium "0.4.4"]]
                   :source-paths ["src" "dev"]
                   :plugins [[lein-cljsbuild "1.1.3"]
                             [lein-doo "0.1.6"]
                             [lein-ancient "0.6.10"]]}}
  :cljsbuild
  {:builds [{:id "test"
             :source-paths ["src" "test"]
             :compiler {:output-to "target/test.js"
                        :optimizations :whitespace
                        :main frag.core-test.runner}}]})
