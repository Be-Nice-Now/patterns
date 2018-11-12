(ns patterns.pipes
  "TODO"
  (:require [clojure.math.combinatorics :as combo]
            [patterns.utils.svg :as svg]
            [patterns.utils :as utils]))

(defn points
  [xn yn width height]
  (concat
    (combo/cartesian-product
      (map #(* % (/ width (inc xn)))
           (range 1 (inc xn)))
      [0 height])
    (combo/cartesian-product
      [0 width]
      (map #(* % (/ height (inc yn)))
           (range 1 (inc yn))))))

(defn- point-pairs
  [[head & items]]
  (->> items
       (map #(remove #{%} items))
       (map point-pairs)
       (zipmap items)
       (reduce (fn [accum [item applesauces]]
                 (concat accum
                         (if (seq applesauces)
                           (map (partial cons [head item])
                                applesauces)
                           [[[head item]]])))
               [])
       vec))

(defn line-sets
  [points]
  (for [pairs (point-pairs points)]
    (for [[start end] pairs]
      (svg/line {:start start
                 :end end}))))

(defn swatches
  [xn yn grid-size]
  (let [width (* (inc xn) grid-size)
        height (* (inc yn) grid-size)]
    (for [line-set (line-sets (points xn yn width height))]
      (utils/veccat
        [:svg
         {:width width
          :height height}
         [:defs {}
          [:style {}
           "line {stroke:rgb(255,0,0);stroke-width:2;}"]]]
        line-set))))
