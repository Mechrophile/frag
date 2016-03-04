(defproject frag "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Unlicense"
            :url "http://unlicense.org/"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.7.228"]
                 [prismatic/plumbing "0.5.2"]
                 [prismatic/schema "1.0.5"]]
  :profiles {:dev {:dependencies [[midje "1.8.3"]]
                   :plugins [[lein-cljsbuild "1.1.2"]
                             [lein-doo "0.1.6"]]}}
  :cljsbuild
  {:builds [{:id "test"
             :source-paths ["src" "test"]
             :compiler {:output-to "target/test.js"
                        :optimizations :whitespace
                        :main frag.core-test.runner}}]}
  )
