(defproject dar/container "0.2.0"
  :description "Dependency injection container"
  :url "https://github.com/dar-clojure/container"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [dar/async.promise "0.1.0"]]
  :profiles {:dev {:dependencies [[criterium "0.4.3"]]
                   :source-paths ["bench"]
                   :aliases {"bench" ["run" "-m" "dar.container.bench"]}}}
   :jvm-opts ^:replace [])
