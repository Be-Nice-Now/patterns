(defproject patterns "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [clojure.java-time "0.3.2"]
                 [com.fzakaria/slf4j-timbre "0.3.12"]
                 [com.taoensso/timbre "4.10.0"]
                 [com.taoensso/tufte "2.0.1"]
                 [org.slf4j/jcl-over-slf4j "1.7.25"]
                 [org.slf4j/jul-to-slf4j "1.7.25"]
                 [org.slf4j/log4j-over-slf4j "1.7.25"]
                 [org.threeten/threeten-extra "1.2"]
                 [hiccup "1.0.5"]
                 [net.mikera/imagez "0.12.0"]
                 [org.apache.xmlgraphics/batik-anim "1.10"]
                 [org.apache.xmlgraphics/batik-codec "1.10"]
                 [org.apache.xmlgraphics/batik-transcoder "1.10"]
                 [org.clojure/data.xml "0.2.0-alpha5"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [rm-hull/clustering "0.2.0"]
                 [rm-hull/inkspot "0.2.1"]]
  :main ^:skip-aot patterns.core
  :target-path "target/%s"
  :jvm-opts ["-Xmx2g"]
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[nu.validator/validator "18.11.5"
                                   :exclusions [xml-apis]]
                                  [xerces/xercesImpl "2.11.0"]]}})
