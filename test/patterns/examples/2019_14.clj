(ns patterns.examples.2019-14
  (:require [patterns.examples :as e]
            [taoensso.tufte :as trace]
            [patterns.tile :as tile]
            [patterns.utils.layout.align :as align]
            [patterns.utils.svg.color :as color]
            [patterns.utils :as utils]
            [patterns.examples.2019-13 :refer [->int vector-field]]))

(defn- tmp-transform-fn
  [tiles-xy idx_1_based day _src [x y] el]
  (let [[tag {el-x :x
              el-y :y
              :keys [xlink:href width height]} & content] el]
    [:g {:transform (format "translate(%s,%s)"
                            el-x el-y)}
     (utils/veccat
       [tag
        {:width width
         :heigh height
         :xlink:href xlink:href
         :transform (format "scale(%s)"
                            (float (/ (vector-field tiles-xy
                                                    idx_1_based
                                                    day
                                                    x y)
                                      360)))}]
       content)]))

(defn gen
  []
  (let [tiles-xy 50
        tile-dimension (-> (/ e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                              tiles-xy)
                           (/ 2)
                           double
                           Math/ceil
                           (* 2)
                           int)
        gen-src (fn [idx_1_based year month day colour]
                  (tile/grid
                    [[:svg {:width tile-dimension
                            :height tile-dimension}
                      [:defs {}]
                      [:circle {:cx (->int (/ tile-dimension
                                              2))
                                :cy (->int (/ tile-dimension
                                              2))
                                :r (->int (/ tile-dimension
                                             2))
                                :style (format "fill:%s;stroke:none;"
                                               (color/map-> colour))}]]]
                    tiles-xy
                    tiles-xy
                    {:transform-fn
                     (fn [_src [x y] el]
                       (let [[tag attrs & content] el]
                         (utils/veccat
                           [tag
                            (assoc attrs
                              :transform (format "scale(%s)"
                                                 (float (/ (vector-field tiles-xy
                                                                         idx_1_based
                                                                         day
                                                                         x y)
                                                           360))))]
                           content)))}))
        gen-fn (fn [idx_1_based year month day background-colour srcs]
                 (e/render [year month day]
                           (align/center
                             (utils/veccat
                               [[:svg {:width e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                                       :height e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT}
                                 [:defs {}]
                                 [:rect {:fill (color/map-> background-colour)
                                         :x 0 :y 0
                                         :width e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                                         :height e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT}]]]
                               (take idx_1_based srcs))
                             {:clip? {:width e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                                      :height e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT}})
                           (e/format-w-newlines
                             [["Riffing off the patterns from last week, same underlying vector field"
                               "but represented as the angle is more extreme by larger circles."]
                              ["As the week progresses, more layers will be added to"
                               "the image. ie, you can tell the day of the week by"
                               "how many colours are present. For instance, today there"
                               "are %s colours (1 is Monday, 2 Tuesday...)."]
                              ["Colours are pulled from popular comics."]
                              ["https://en.wikipedia.org/wiki/Vector_field"]]
                             idx_1_based)
                           {:recursive? false}))
        [background & palette] (->> e/poke-palettes
                                    (filter (fn [palette]
                                              (>= 8 (count palette))))
                                    (shuffle)
                                    first
                                    (map second))
        srcs (for [[[idx_1_based year month day] colour]
                   (map list
                        (e/indexed-days-of-week (e/week->date 2019 13))
                        palette)]
               (gen-src idx_1_based year month day colour))]
    (doseq [[idx_1_based year month day] (e/indexed-days-of-week (e/week->date 2019 14))]
      (gen-fn idx_1_based year month day background srcs))))

(comment
  (trace/profile {} (gen)))
