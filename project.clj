(defproject patterns "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/data.xml "0.2.0-alpha5"]
                 [crouton "0.1.2"]
                 [dali "0.7.4"]
                 [hiccup "1.0.5"]]
  :main ^:skip-aot patterns.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
