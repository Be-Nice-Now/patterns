(ns patterns.examples.2019-23
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

(defn gen
  []
  (let [gen-fn (fn [_idx_1_based year month day [background-color zero-color one-color]]
                 (let []
                   (e/render [year month day]
                             (align/center
                               [(rect e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                                      background-color)
                                (numbers-svg [year month day 0]
                                             zero-color
                                             one-color)]
                               {:clip? {:width e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                                        :height e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT}})
                             (e/format-w-newlines
                               [["Binary representations of the year, month, day, and"
                                 "leap second...there's rarely one of those though..."]
                                [year month day 0]
                                ["https://en.wikipedia.org/wiki/Leap_second"]]))))
        palettes (->> e/poke-palettes
                      (filter (fn [palette]
                                (<= 3 (count palette))))
                      (map (fn [palette]
                             (map second palette)))
                      shuffle)]
    (doseq [[[idx_1_based year month day] palette]
            (map list
                 (e/indexed-days-of-week (e/week->date 2019 23))
                 palettes)]
      (gen-fn idx_1_based year month day palette))))

(comment
  (trace/profile {} (gen)))


