(ns patterns.hiccup
  (:require [clojure.data.xml :as xml]
            [clojure.string :as str]
            [patterns.utils :as utils]))

(def xmlns
  {:href :xlink:href})

(defn- xml-tag->hiccup
  [kw]
  (let [k ((comp keyword name) kw)]
    (or (k xmlns)
        k)))

(defn- string->hiccup
  [s]
  (let [trimmed-string (-> s
                           str/trim
                           str/trim-newline)]
    (and (seq trimmed-string)
         (or (try
               (Integer/valueOf ^String trimmed-string)
               (catch Exception e
                 false))
             (try
               (Float/valueOf ^String trimmed-string)
               (catch Exception e
                 false))
             trimmed-string))))

(defn- style->hiccup
  [style-str]
  (map (fn [s]
         (let [parsed (string->hiccup s)]
           (when (seq parsed)
             (str parsed "}"))))
       (str/split style-str #"\}")))

(defn- attrs->hiccup
  [attrs]
  (into {}
        (map (fn [[k v]]
               [(xml-tag->hiccup k)
                (string->hiccup v)]))
        attrs))

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
       (attrs->hiccup (or attrs
                          {}))]
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

(defn file->hiccup
  [f]
  (-> f
      slurp
      svg->hiccup))
