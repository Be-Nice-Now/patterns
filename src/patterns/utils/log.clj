(ns patterns.utils.log
  (:require [clojure.string :as str]
            [taoensso.timbre :as log])
  (:import [java.util.logging Level
                              Logger
                              LogManager]
           [org.slf4j.bridge
            SLF4JBridgeHandler]))

(defn deep-merge
  [& maps]
  (apply merge-with
         (fn [x y]
           (cond (map? y) (deep-merge x y)
                 (vector? y) (concat x y)
                 :else y))
         maps))

(defn- bridge-jul-to-timbre
  "java.util.Logging is more stubborn than other bridges
  to SLF4J and requires additional maintenance to get setup.
  Relevant links to this can be found:
  https://stackoverflow.com/a/9117188"
  []
  (.reset (LogManager/getLogManager))
  (SLF4JBridgeHandler/removeHandlersForRootLogger)
  (SLF4JBridgeHandler/install)
  (.setLevel (Logger/getLogger "global")
             Level/ALL))
(bridge-jul-to-timbre)

(defn default-output-fn
  "Just like Timbre's default-output-fn, except this adds in the MDC."
  ([data]
   (default-output-fn nil data))
  ([opts {ctx :context
          :as data}]
   (-> (log/default-output-fn opts data)
       ;; \S+ timestamp
       ;; \S+ hostname
       ;; \S+ level
       ;; <-- injected context
       ;; [... rest
       (str/replace-first #"(\S+ \S+ \S+) \["
                          (str "$1 " ctx " [")))))

(defn init!
  []
  (log/merge-config!
    {:level :debug
     :output-fn default-output-fn}))
(init!)


(defmacro  with-context
  "Exactly like `taoensso.timbre/with-context`, except `deep-merge`s
   `context` with existing `taoensso.timbre/*context*`"
  [context & body] `(binding [log/*context* (deep-merge log/*context*
                                                        ~context)]
                      ~@body))
