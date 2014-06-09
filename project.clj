(defproject dar/container "0.1.0-SNAPSHOT"
  :description "Dependency injection container"
  :url "https://github.com/dar-clojure/core"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [dar/async "0.1.0"]]
  :profiles {:dev {:dependencies [[criterium "0.4.3"]]
                   :source-paths ["bench"]
                   :aliases {"bench" ["run" "-m" "dar.container.bench"]}}}
   :jvm-opts ^:replace [])
