(ns patterns.hiccup
  (:require [clojure.data.xml :as xml]
            [clojure.string :as str]
            [patterns.utils :as utils]))

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

(defn style->hiccup
  [style-str]
  (map (fn [s]
         (let [parsed (string->hiccup s)]
           (when (seq parsed)
             (str parsed "}"))))
       (str/split style-str #"\}")))

(defn xml-element->hiccup
  "Given an XML Element, translate it to Hiccup."
  [{:keys [tag attrs content] :as el}]
  (let [parsed-tag (xml-tag->hiccup tag)
        content->hiccup (fn [x]
                          (cond
                            (map? x) [(xml-element->hiccup x)]
                            (and (= :style parsed-tag)
                                 (string? x)) (style->hiccup x)
                            (string? x) [(string->hiccup x)]
                            :else nil))]
    (utils/veccat
      [parsed-tag
       (or attrs
           {})]
      (->> content
           (mapcat content->hiccup)
           (filter identity)))))

(defn svg->hiccup
  "Given an SVG encoded string `s`, parse into the Hiccup format.
   Useful for working with SVGs which have been manipulated outside
   this toolset."
  [s]
  (->> s
       xml/parse-str
       xml-element->hiccup))
