(ns patterns.utils.layout.align
  (:require [patterns.utils :as utils]
            [patterns.utils.svg :as svg]))

(defn- ->int
  [n]
  (-> n
      float
      Math/round
      int))

(defn center
  [srcs & [{:keys [clip?]
            :or {clip? false}}]]
  (let [{w :width
         h :height} (or clip?
                        {:width (->int (apply max (map (comp :width svg/dimensions)
                                                       srcs)))
                         :height (->int (apply max (map (comp :height svg/dimensions)
                                                        srcs)))})
        gen-id (partial gensym "center")
        ids (repeatedly (count srcs) gen-id)]
    (utils/veccat
      [:svg {:width w
             :height h}
       (utils/veccat
         [:defs {}]
         (map svg/->def
              srcs
              ids))]
      (map (fn [src id]
             (let [{:keys [width height]} (svg/dimensions src)
                   gap-width (->int (/ (- w width)
                                       2))
                   gap-height (->int (/ (- h height)
                                        2))]
               (svg/use id
                        {:x gap-width
                         :y gap-height})))
           srcs
           ids))))
