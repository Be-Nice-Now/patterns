(ns patterns.test-utils
  (:require [patterns.hiccup :as hiccup]
            [patterns.core :as patterns])
  (:import [java.io StringReader]
           [nu.validator.validation SimpleDocumentValidator]
           [org.xml.sax ErrorHandler
                        InputSource]))

(defn validation-errors
  [svg-html-string]
  (let [validation-errors (atom [])
        checker (doto (SimpleDocumentValidator. false false false)
                  (.setUpMainSchema "http://s.validator.nu/svg-xhtml5-rdf-mathml.rnc"
                                    (reify ErrorHandler))
                  (.setUpValidatorAndParsers (reify ErrorHandler
                                               (warning [_this ex]
                                                 (swap! validation-errors conj
                                                        {:type :warning
                                                         :ex ex}))
                                               (error [_this ex]
                                                 (swap! validation-errors conj
                                                        {:type :error
                                                         :ex ex}))
                                               (fatalError [_this ex]
                                                 (swap! validation-errors conj
                                                        {:type :fatal
                                                         :ex ex})))
                                             false
                                             false))]
    (.checkXmlInputSource checker (doto (InputSource. (StringReader. svg-html-string))
                                    (.setSystemId "SVG-STRING-CHECKER")))
    @validation-errors))

(defn hiccup-validation-errors
  [src]
  (-> src
      (patterns/render)
      validation-errors))

(defn resource->hiccup
  [n]
  (-> n
      clojure.java.io/resource
      slurp
      hiccup/svg->hiccup))
