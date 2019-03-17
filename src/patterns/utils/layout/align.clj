(ns patterns.utils.layout.align
  (:require [patterns.utils :as utils]
            [patterns.utils.svg :as svg]))

(defn center
  [[src & srcs]]
  (let [{w :width
         h :height
         :as dims} (svg/dimensions src)
        gen-id (partial gensym "center")
        id (gen-id)
        ids (repeatedly (count srcs) gen-id)]
    (utils/veccat
      [:svg dims
       (utils/veccat
         [:defs {}
          (svg/->def src id)]
         (map svg/->def
              srcs
              ids))
       (svg/use id {})]
      (map (fn [src id]
             (let [{:keys [width height]} (svg/dimensions src)
                   gap-width (float (/ (- w width)
                                       2))
                   gap-height (float (/ (- h height)
                                        2))]
               (svg/use id
                        {:x gap-width
                         :y gap-height})))
           srcs
           ids))))
