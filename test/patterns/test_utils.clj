(ns patterns.test-utils
  (:require [patterns.hiccup :as hiccup]
            [patterns.core :as patterns])
  (:import [org.xml.sax ErrorHandler
                        InputSource]
           [java.io StringReader]
           [nu.validator.messages MessageEmitterAdapter]
           [nu.validator.client EmbeddedValidator$OutputFormat]))

(comment
  nu.validator.client.SimpleCommandLineValidator)


;; Generate SVGs using patterns, then turn them into strings using hiccup, then validate them with:
;;; https://github.com/validator/validator/blob/fadbfed45f9439933e600e2d37029ad518c4265e/src/nu/validator/client/SimpleCommandLineValidator.java#L420-L427
;;;  private static SimpleDocumentValidator validator;
;;;

;;; checksvgfile
;;;; setschema

(defn validation-errors
  [svg-html-string]
  (let [validation-errors (atom [])
        checker (doto (nu.validator.validation.SimpleDocumentValidator. false false false)
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
      hiccup-validation-errors))

(defn resource->hiccup
  [n]
  (-> n
      clojure.java.io/resource
      slurp
      hiccup/svg->hiccup))
