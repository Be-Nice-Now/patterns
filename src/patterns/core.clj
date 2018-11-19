(ns patterns.core
  (:require [hiccup.core :as html]
            [patterns.utils.svg :as svg]))

(defn render
  "Given a `src` Hiccup svg, return a string HTML representation.
   If provided a filename, place the rendered representation there."
  ([src]
   (->> src
        svg/to-svg-doc
        html/html))
  ([filename src]
   (spit (render src) filename)))
