(ns patterns.examples.2019-24
  (:require [patterns.utils.svg.color :as color]
            [patterns.transform :as transform]
            [patterns.tile :as tile]
            [taoensso.tufte :as trace]
            [patterns.examples :as e]
            [patterns.graphs :as graphs]
            [patterns.utils :as utils]
            [patterns.examples.2019-13 :refer [->int]]
            [patterns.utils.svg :as svg]
            [patterns.utils.layout.align :as align]))

(defn ->ceil
  [n]
  (-> n
      float
      Math/ceil
      int))

(defn blank
  [wh]
  [:svg {:width wh
         :height wh}
   [:defs {}]])

(defn rect
  [wh background-color]
  [:svg {:width wh
         :height wh}
   [:defs {}]
   [:rect {:fill (color/map-> background-color)
           :x 0 :y 0
           :height wh
           :width wh}]])

(defn to-binary-array
  [n]
  (->> (Long/toBinaryString ^Long n)
       seq
       (map {\1 1 \0 0})))

(defn numbers-to-binary-arrays
  [numbers]
  (utils/veccat
    (map to-binary-array numbers)))

(defn numbers-to-tiles
  [numbers zero-color one-color]
  (let [binary-arrays (numbers-to-binary-arrays numbers)
        length (+ (apply + (map count binary-arrays))
                  (dec (count binary-arrays)))
        wh (->ceil (/ e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                      length))
        to-tile (fn [n]
                  (rect wh (if (zero? n)
                             zero-color
                             one-color)))
        to-tiles (fn [s]
                   (map to-tile s))]
    (apply utils/veccat
           (interpose [(blank wh)]
                      (map to-tiles binary-arrays)))))

(defn numbers-svg
  [numbers zero-color one-color]
  (let [tiles (numbers-to-tiles numbers zero-color one-color)]
    (def snag [numbers zero-color one-color tiles])
    (tile/grid
      tiles
      (count tiles) 1)))

(defn offsets
  [n]
  (reduce (fn [accum offset]
            (conj accum (+ (last accum) offset)))
          [0]
          (repeatedly n
                      (fn []
                        (rand-nth [0 1 -1])))))

(defn stripe
  [width bar-data]
  (let [bars (->> bar-data
                  (map (fn [[k percent]]
                         [k (Math/round ^Float (* width percent))]))
                  (remove (comp zero? second))
                  (take-last width))
        ->rect (fn [{:keys [x width]} [bar-color bar-width]]
                 (conj [:rect {:fill (color/map-> bar-color)
                               :x (+ x width)
                               :y 0
                               :height 1
                               :width bar-width}]))]
    (utils/veccat
      [:svg
       {:width width
        :height 1}
       [:defs {}]]
      (reduce (fn [accum bar]
                (conj accum
                      (->rect (-> accum
                                  last
                                  second)
                              bar)))
              [(->rect {:x 0 :width 0} (first bars))]
              (rest bars)))))

(defn palette->svg
  [palette]
  (align/center
    [[:svg {:width e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
            :height 1}
      [:defs {}]
      [:rect {:fill (-> palette
                        first
                        second
                        color/map->)
              :x 0 :y 0
              :height 1
              :width e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT}]
      [:rect {:fill (-> palette
                        last
                        second
                        color/map->)
              :x e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT :y 0
              :height 1
              :width e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT}]]
     (svg/->def (stripe e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT (map rest palette))
                "to-rotate")]))

(defn use-offsets
  [src offsets]
  (utils/veccat
    [:svg (assoc (svg/dimensions src)
            :height (count offsets))
     [:defs {}
      (svg/->def src "slice")]]
    (map-indexed (fn [idx offset]
                   (svg/use "slice" {:x offset
                                     :y idx}))
                 offsets)))

(defn gen
  []
  (let [gen-fn (fn [_idx_1_based year month day palette]
                 (e/render [year month day]
                           (use-offsets
                             (palette->svg (shuffle palette))
                             (offsets e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT))
                           (e/format-w-newlines
                             [["Wavy."]])))
        palettes (shuffle e/poke-palettes)]
    (doseq [[[idx_1_based year month day] palette]
            (map list
                 (e/indexed-days-of-week (e/week->date 2019 24))
                 palettes)]
      (gen-fn idx_1_based year month day palette))))

(comment
  (trace/profile {} (gen)))


