(ns patterns.examples.2019-13
  (:require [patterns.examples :as e]
            [taoensso.tufte :as trace]
            [patterns.tile :as tile]
            [patterns.utils.layout.align :as align]
            [patterns.utils.svg.color :as color]
            [patterns.utils :as utils]))

(defn- ->int
  [n]
  (-> n
      float
      Math/round
      int))

(defn vector-field
  [tiles-xy idx day x y]
  (+ (* (->int (* x (/ 360
                       tiles-xy)))
        (dec idx))
     (->int (* y (/ 360
                    tiles-xy)))))

(defn gen
  []
  (let [tiles-xy 50
        tile-stroke-width 2
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
                      [:line {:x1 (int (/ tile-dimension
                                          2))
                              :y1 0
                              :x2 (int (/ tile-dimension
                                          2))
                              :y2 tile-dimension
                              :stroke-width tile-stroke-width
                              :stroke (color/map-> colour)}]]]
                    tiles-xy
                    tiles-xy
                    {:transform-fn
                     (partial
                       tile/transform-rotate
                       (fn [_src [x y] _element]
                         (vector-field tiles-xy
                                       idx_1_based
                                       day
                                       x y)))}))
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
                             [["Playing with simplistic vector fields this week."
                               "As the week progresses, more layers will be added to"
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
    (doseq [[idx_1_based year month day] (e/indexed-days-of-week (e/week->date 2019 13))]
      (gen-fn idx_1_based year month day background srcs))))

(comment
  (trace/profile {} (gen)))

