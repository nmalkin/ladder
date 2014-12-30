(defproject ladder "HEAD-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :main ^:skip-aot ladder.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
