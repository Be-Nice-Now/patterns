(ns patterns.utils.svg.rotate
  (:require [patterns.utils.svg :as svg]))

(defn center
  [src degree]
  (let [{:keys [width height]
         :as dims} (svg/dimensions src)
        id (gensym "rotate")]
    [:svg dims
     [:defs {}
      (svg/->def src id)]
     (svg/use id
              {:transform (format "rotate(%s,%s,%s)"
                                  degree
                                  (Math/round (float
                                                (/ width 2)))
                                  (Math/round (float
                                                (/ height 2))))})]))
