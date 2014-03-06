(defproject easy-app/core "0.1.0-SNAPSHOT"
  :description "Dependency injection for Clojure"
  :url "http://github.com/eldargab/easy-app.clj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [easy-app/async "0.1.0-SNAPSHOT"]]
  :profiles {:dev {:dependencies [[criterium "0.4.3"]]
                   :source-paths ["bench"]
                   :aliases {"bench" ["run" "-m" "easy-app.core.bench"]}}}
   :jvm-opts ^:replace [])
