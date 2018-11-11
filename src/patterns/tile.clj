(ns patterns.tile
  (:require [patterns.utils :as utils]
            [patterns.utils.svg :as svg]))

(defn grid
  "Given a src Hiccup svg, return a Hiccup svg where it is
   tiled xn by yn times."
  [src xn yn]
  (let [{:keys [width height]} (svg/dimensions src)]
    (utils/veccat [:svg
                   {:xmlns "http://www.w3.org/2000/svg"
                    :viewBox (format "0 0 %s %s"
                                     (* xn width)
                                     (* yn height))}
                   [:defs
                    (svg/->defs src "base-tile")]]
                  (for [x (range 0 xn)
                        y (range 0 yn)]
                    (svg/use "base-tile"
                             (* x width)
                             (* y height))))))
