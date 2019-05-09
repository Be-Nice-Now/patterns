(ns patterns.examples.2019-17
  (:require [patterns.examples :as e]
            [taoensso.tufte :as trace]
            [patterns.tile :as tile]
            [patterns.utils.svg.color :as color]
            [patterns.utils :as utils]
            [patterns.examples.2019-13 :refer [->int vector-field]]
            [taoensso.timbre :as log]
            [patterns.transform :as transform]
            [patterns.graphs :as graphs]
            [clojure.java.io :as io]))

(defn gen
  []
  (let [wh (/ e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT 2 10)
        tile-dims {:width wh
                   :height wh}
        filled-tile-gen (fn [colour-gen]
                          (let [colour (colour-gen)]
                            [:svg tile-dims
                             [:defs {}]
                             [:rect (assoc tile-dims
                                      :x 0 :y 0
                                      :style (format "fill:%s;"
                                                     (color/map-> colour)))]]))
        svg-swatches (->> "./doc/2019-17/"
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
        palette (graphs/histogram-data (first svg-swatches))
        colour-gen (palette-to-colour-gen palette)]
    (e/render [2019 5 8]
              (tile/grid
                (conj svg-swatches
                      (tile/grid
                        (repeatedly 100
                                    (partial filled-tile-gen colour-gen))
                        10 10))
                2 2)
              (e/format-w-newlines
                [["Frame view of \"simple gear-based cube fidget\""
                  "from monday."
                  "The bottom right panel is generated from the palette of the top left."]
                 ["This week we're working with the @bricklinkstudio app from"
                  "@bricklink, which allows users to model designs with our"
                  "favourite building material: #LEGO. This is a rendering"
                  "of a simple gear-based cube fidget that we will submit"
                  "to @mochub for iteration."]
                 ["Throughout the week, we will post different views of this"
                  "mockup, its interior, and the building process."]
                 ["#creative #brickorbust #gears #mechanical"]]))))

(comment
  (trace/profile {} (gen)))

