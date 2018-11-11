(ns patterns.hiccup
  (:require [clojure.data.xml :as xml]
            [clojure.string :as str]))

(defn- xml-tag->hiccup
  [kw]
  ((comp keyword name) kw))

(defn- string->hiccup
  [s]
  (let [ret
        (-> s
            str/trim
            str/trim-newline)]
    (and (seq ret)
         ret)))

(defn xml-element->hiccup
  "Given an XML Element, translate it to Hiccup."
  [{:keys [tag attrs content]}]
  (let [content->hiccup (fn [x]
                          (cond
                            (map? x) (xml-element->hiccup x)
                            (string? x) (string->hiccup x)
                            :else nil))]
    (->> (concat [(xml-tag->hiccup tag)
                  (or attrs
                      {})]
                 (map content->hiccup content))
         (filter identity)
         vec)))

(defn svg->hiccup
  "Given an SVG encoded string `s`, parse into the Hiccup format.
   Useful for working with SVGs which have been manipulated outside
   this toolset."
  [s]
  (->> s
       xml/parse-str
       xml-element->hiccup))
