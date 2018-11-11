(ns patterns.core
  (:require [hiccup.core :as html]
            [patterns.hiccup :as hiccup]
            [patterns.utils.svg :as svg]
            [patterns.tile :as tile]))

(defn render
  [filename src]
  (->> src
       svg/to-svg-doc
       html/html
       (spit filename)))

(comment
  (render
    "./pipes-04.svg"
    (tile/grid (hiccup/svg->hiccup (slurp "./pipes-01.svg"))
               100 100)))
