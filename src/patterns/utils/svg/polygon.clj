(ns patterns.utils.svg.polygon
  (:require [clojure.string :as str]))

(defn equilateral
  ([edges radius]
    (equilateral edges radius {}))
  ([edges radius attrs]
   [:polygon (assoc attrs
               :points (->> (range 0 (* 2 Math/PI) (/ (* 2 Math/PI) edges))
                            (take edges)
                            (map (fn [degree]
                                   (format "%s,%s"
                                           (+ (int (* radius (Math/cos (double degree))))
                                              radius)
                                           (+ (int (* radius (Math/sin (double degree))))
                                              radius))))
                            (str/join " ")))]))
