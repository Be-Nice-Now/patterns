(ns patterns.examples.2019-21
  (:require [clojure.java.io :as io]
            [patterns.utils.svg.color :as color]
            [patterns.transform :as transform]
            [patterns.tile :as tile]
            [taoensso.tufte :as trace]
            [patterns.examples :as e]
            [patterns.graphs :as graphs]))

(defn ->int
  [n]
  (-> n
      float
      Math/ceil
      int))

(defn filled-tile-gen
  [tile-dim colour-gen]
  (let [colour (colour-gen)]
    [:svg {:height tile-dim :width tile-dim}
     [:defs {}]
     [:rect {:height tile-dim
             :width tile-dim
             :x 0 :y 0
             :style (format "fill:%s;"
                            (color/map-> colour))}]]))

(defn filled-tiles
  [xy colour-gen]
  (tile/grid
    (repeatedly
      (* xy xy)
      (partial filled-tile-gen
               (->int (/ e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                         2
                         xy))
               colour-gen))
    xy xy))

(defn gen
  []
  (let [k 10
        svg-swatches (->> "./doc/2019-21/"
                          io/file
                          file-seq
                          rest
                          (map e/path->svg)
                          (into []))
        palette-to-colour-gen (fn [palette]
                                (let [hist-colours (mapv first palette)
                                      colour-gen-gen (transform/bin-idx (map second palette))]
                                  (fn []
                                    (hist-colours (colour-gen-gen (rand))))))

        gen-fn (fn [idx_1_based year month day]
                 (let [svg-swatch (nth svg-swatches (dec idx_1_based))
                       raster-svg-swatch (e/path->svg (transform/rasterize svg-swatch k))

                       palette (->> raster-svg-swatch
                                    graphs/histogram-data
                                    (sort-by second)
                                    (take-last k))
                       colour-gen (palette-to-colour-gen palette)
                       quarter (/ e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                                  2)]
                   (e/render [year month day]
                             (tile/grid
                               (mapv (fn [src]
                                       (e/center {:width quarter
                                                  :height quarter}
                                                 src))
                                     [svg-swatch
                                      (filled-tiles 2 colour-gen)
                                      (filled-tiles 4 colour-gen)
                                      (filled-tiles 8 colour-gen)])
                               2 2)
                             (e/format-w-newlines
                               [["This past week we had the fortune of"
                                 "visiting #LA to see off someone very dear"
                                 "to us @sweatyroosevelt."]
                                ["Source photos were shot by the very talented @ktmackkk!"]]))))]
    (doseq [[idx_1_based year month day] (e/indexed-days-of-week (e/week->date 2019 21))]
      (gen-fn idx_1_based year month day))))

(comment
  (trace/profile {} (gen)))

