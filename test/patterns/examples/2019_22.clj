(ns patterns.examples.2019-22
  (:require [clojure.java.io :as io]
            [patterns.utils.svg.color :as color]
            [patterns.transform :as transform]
            [patterns.tile :as tile]
            [taoensso.tufte :as trace]
            [patterns.examples :as e]
            [patterns.graphs :as graphs]
            [patterns.utils :as utils]
            [patterns.examples.2019-13 :refer [->int]]
            [patterns.utils.svg :as svg]
            [patterns.utils.layout.align :as align]))

(def stroke-width 6)

(defn ->ceil
  [n]
  (-> n
      float
      Math/ceil
      int))

(defn ->floor
  [n]
  (-> n
      float
      Math/floor
      int))

(defn gen-base
  [wh stroke-color]
  (let [cross-xy (->int (/ wh 2))
        styles {:stroke stroke-color
                :stroke-width stroke-width}]
    [:svg {:width wh :height wh}
     [:defs {}]
     [:line (assoc styles
              :x1 cross-xy
              :y1 0
              :x2 cross-xy
              :y2 wh)]
     [:line (assoc styles
              :x1 0
              :y1 cross-xy
              :x2 wh
              :y2 cross-xy)]]))

(defn cross-hatch-gradient
  [wh n stroke-color]
  (let [points (e/cross-hatch-points wh n)
        line-fn (fn [point]
                  [:line {:x1 0 :y1 point
                          :x2 wh :y2 point
                          :stroke-width stroke-width
                          :stroke stroke-color}])]
    (utils/veccat
      [:svg {:width wh :height wh}
       [:defs {}]]
      (mapv line-fn points))))

(defn gen-to-slice
  [wh center-color stroke-color cross-hatch-density]
  (let [outline-stroke-width (->floor (/ stroke-width
                                         (Math/sqrt 2)))
        id (gensym "ch")
        clip-id (gensym "c")
        wh2 (->int (/ wh 2))
        path {:d (format "M %s %s L %s %s L %s %s L %s %s Z"
                         0 wh2
                         wh2 0
                         wh wh2
                         wh2 wh)
              :fill "none"
              :stroke-linejoin "miter"}]
    [:svg {:width wh :height wh}
     [:defs {}
      (svg/->def (cross-hatch-gradient
                   wh
                   cross-hatch-density
                   stroke-color)
                 id)
      [:clipPath {:id clip-id}
       [:path path]]]
     (utils/veccat
       [:g {:clip-path (format "url(#%s)"
                               clip-id)}]

       (if center-color
         [[:rect {:fill center-color
                  :x 0 :y 0
                  :height wh
                  :width wh}]]
         [])
       [(svg/use id {})])
     [:path
      (assoc path
        :stroke stroke-color
        :stroke-width outline-stroke-width)]]))

(defn quarter-panels
  [src]
  (let [wh (-> src
               svg/dimensions
               :width
               (/ 2)
               ->int)
        id (gensym "s")]
    (for [x [0 wh]
          y [0 wh]]
      [:svg {:width wh :height wh}
       [:defs {}
        (svg/->def src (format "%s--%s-%s" id x y))]
       (svg/use (format "%s--%s-%s" id x y)
                {:x (- x)
                 :y (- y)})])))

(defn blank
  [src]
  [:svg (svg/dimensions src)
   [:defs {}]])

(defn tile-gen
  [src base]
  (let [panels (quarter-panels src)]
    (align/center
      [base
       (tile/grid
         (utils/veccat
           (repeat 4 (blank (first panels)))
           panels)
         2 2
         {:transform-fn tile/transform-rotate
          :pick-fn tile/pick-random})])))

(defn tiles
  [n wh center-color stroke-color cross-hatch-density]
  (let [src (gen-to-slice wh center-color stroke-color cross-hatch-density)
        base (gen-base wh stroke-color)]
    (conj (->> (repeatedly (* 2 n)
                           (partial tile-gen src base))
               set
               (take (dec n)))
          (align/center
            [base
             src]))))

(defn gen
  []
  (let [tiles-xy 20
        tile-wh (/ e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                   tiles-xy)
        gen-fn (fn [idx_1_based year month day [background-color center-color stroke-color]]
                 (let [tiles-to-gen (+ idx_1_based month)
                       t (tiles tiles-to-gen
                                tile-wh
                                (color/map-> center-color)
                                (color/map-> stroke-color)
                                day)]
                   (e/render [year month day]
                             (align/center
                               [[:svg {:width e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                                       :height e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT}
                                 [:defs {}]
                                 [:rect {:fill (color/map-> background-color)
                                         :x 0 :y 0
                                         :height e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                                         :width e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT}]]
                                (tile/grid
                                  t
                                  tiles-xy tiles-xy
                                  {
                                   ;:transform-fn tile/transform-rotate
                                   :pick-fn tile/pick-random})])
                             (e/format-w-newlines
                               [["Images this week are generated from %s"
                                 "source tiles. ie, the index of the day of the week (%s)"
                                 "offset by the month (%s). The fill of these tiles"
                                 "increases with %s cross hatches, representing the day"
                                 "of the month."]
                                ["Inspiration for this came from one of our"
                                 "collaborators' studies with"
                                 "@helen_louise_allen_textiles"
                                 "at the @uwsohe."]
                                ["Colours are sourced from popular comics."]]
                               tiles-to-gen idx_1_based month day))))
        palettes (->> e/poke-palettes
                      (filter (fn [palette]
                                (<= 3 (count palette))))
                      (map (fn [palette]
                             (map second palette)))
                      shuffle)]
    (doseq [[[idx_1_based year month day] palette]
            (map list
                 (e/indexed-days-of-week (e/week->date 2019 22))
                 palettes)]
      (gen-fn idx_1_based year month day palette))))

(comment
  (trace/profile {} (gen)))
