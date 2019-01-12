(ns patterns.fabrics
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [mikera.image.core :as img]
            [patterns.transform :as transform]
            [patterns.tile :as tile]
            [patterns.core :as patterns]
            [patterns.pipes :as pipes]
            [patterns.utils.paths :as utils.paths]
            [patterns.utils.svg :as svg]
            [clojure.string :as str]
            [patterns.shatter :as shatter]))

(defn cmyk-rgb
  [tile-width]
  (let [swatch-dimension (bit-shift-left 1 8)
        colour-blocks (concat [[:rect {:fill "rgb(0,0,0)"
                                       :x 0 :y 0
                                       :height swatch-dimension
                                       :width swatch-dimension}]]
                              (for [i (range 3)]
                                [:rect {:fill (str "rgb("
                                                   (str/join "," (assoc [0 0 0] i 255))
                                                   ")")
                                        :x (int (* swatch-dimension
                                                   (/ 3 8)))
                                        :y (int (* swatch-dimension
                                                   (/ 3 8)))
                                        :height (/ swatch-dimension 4)
                                        :width (/ swatch-dimension 4)}])
                              (for [i (range 3)]
                                [:rect {:fill (str "rgb("
                                                   (str/join "," (assoc [255 255 255] i 0))
                                                   ")")
                                        :x (/ swatch-dimension 4) :y (/ swatch-dimension 4)
                                        :height (/ swatch-dimension 2)
                                        :width (/ swatch-dimension 2)}])
                              [[:rect {:fill "rgb(255,255,255)"
                                       :x 0 :y 0
                                       :height swatch-dimension
                                       :width swatch-dimension}]])
        colour-blocks-swatches (map (fn [rect]
                                      [:svg
                                       {:height swatch-dimension
                                        :width swatch-dimension}
                                       [:defs {}]
                                       rect])
                                    colour-blocks)
        colour-blocks-tiled (tile/grid
                              colour-blocks-swatches
                              tile-width 1)
        swatch-dimension--w-padding (int (* swatch-dimension
                                            (/ 60 64)))
        shapes (concat [[:circle {:cx 0 :cy 0 :r (/ swatch-dimension--w-padding
                                                    2)}]
                        [:line {:x1 (- (/ swatch-dimension--w-padding 8)) :y1 0
                                :x2 (/ swatch-dimension--w-padding 8) :y2 0
                                :stroke-width (/ swatch-dimension--w-padding 8)
                                :stroke-linecap "round"}]]
                       (for [points (range 3 (inc tile-width))]
                         ;; y = mx + b
                         ;; (x0, y0) = (3, (/ swatch-dimension--w-padding 4))
                         ;; (xn, yn) = (tile-width, swatch-dimension--w-padding)
                         (let [min-radius (/ swatch-dimension--w-padding 8)
                               max-radius (/ swatch-dimension--w-padding 2)
                               rise (- max-radius min-radius)
                               run (- tile-width 3)
                               m (/ rise run)
                               b (- max-radius (* tile-width m))
                               radius (+ (* m points)
                                         b)]
                           [:polygon {:points (->> (range 0
                                                          (* 2 Math/PI)
                                                          (/ (* 2 Math/PI) points))
                                                   (take points)
                                                   (map (fn [radians]
                                                          (format "%s,%s"
                                                                  (int (* radius
                                                                          (Math/cos (double radians))))
                                                                  (int (* radius
                                                                          (Math/sin (double radians)))))))
                                                   (str/join " "))}])))
        shapes-swatches (map (fn [[tag attrs]]
                               [:svg
                                {:height swatch-dimension
                                 :width swatch-dimension}
                                [:defs {}]
                                [tag (assoc attrs
                                       :transform (format "translate(%s, %s)"
                                                          (/ swatch-dimension 2)
                                                          (/ swatch-dimension 2))
                                       :fill "rgb(0,0,0)"
                                       :stroke "rgb(0,0,0)")]])
                             shapes)
        shapes-tiled (tile/grid
                       shapes-swatches
                       tile-width 1)
        {:keys [style path-fn]} (svg/multi-stroke [{:width (int (* swatch-dimension
                                                                   (/ 4 8)))
                                                    :colour {:r 255 :g 255 :b 255}}])
        pipes-swatches (pipes/swatches
                         1 1
                         {:line-fn (fn [& args]
                                     (path-fn (apply svg/quadratic args)))
                          :style style
                          :grid-size (/ swatch-dimension 2)})
        tile-height (count colour-blocks)
        pipes-tiled (tile/grid
                      pipes-swatches
                      tile-width tile-height
                      {:transform-fn tile/transform-rotate})
        colour-blocks-id (gensym "id")
        shapes-id (gensym "id")
        pipes-id (gensym "id")]
    [:svg
     {:height (* (+ tile-height 2) swatch-dimension)
      :width (* tile-width swatch-dimension)}
     [:defs {}
      (svg/->def (shatter/dark shapes-tiled (int (/ swatch-dimension
                                                    32)))
                 shapes-id)
      (svg/->def (shatter/dark
                   [:svg {:height (* tile-height swatch-dimension)
                          :width (* tile-width swatch-dimension)}
                    [:defs {}]
                    [:rect {:fill "rgb(0,0,0)"
                            :x 0 :y 0
                            :height (* tile-height swatch-dimension)
                            :width (* tile-width swatch-dimension)}]
                    pipes-tiled]
                   (int (/ swatch-dimension
                           64)))
                 pipes-id)
      (svg/->def colour-blocks-tiled colour-blocks-id)]
     [:rect {:fill "rgb(255,255,255)"
             :x 0 :y 0
             :height (* (+ tile-height 2) swatch-dimension)
             :width (* tile-width swatch-dimension)}]
     (svg/use shapes-id
              {})
     (svg/use pipes-id
              {:y swatch-dimension})
     (svg/use colour-blocks-id
              {:y (* (inc tile-height) swatch-dimension)})]))

(comment
  (patterns/render
    "./cmyk-long-64-rgb"
    (cmyk-rgb 64)
    :png))
