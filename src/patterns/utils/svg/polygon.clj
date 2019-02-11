(ns patterns.utils.svg.polygon
  (:require [clojure.string :as str]))

(defn- round
  [n]
  (float
    (/ (Math/round (float (* 1000 n)))
       1000)))

(defn equilateral
  ([edges radius]
   (equilateral edges radius {}))
  ([edges radius attrs]
   [:polygon (assoc attrs
               :points (->> (range 0 (* 2 Math/PI) (/ (* 2 Math/PI) edges))
                            (take edges)
                            (map (fn [degree]
                                   [(Math/cos degree)
                                    (Math/sin degree)]))
                            flatten
                            (map (comp round (partial * radius) inc))
                            (partition 2)
                            (map (fn [[x y]]
                                   (format "%s,%s"
                                           x y)))
                            (str/join " ")))]))
