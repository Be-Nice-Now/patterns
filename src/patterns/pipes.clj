(ns patterns.pipes
  "TODO"
  (:require [clojure.math.combinatorics :as combo]
            [patterns.utils.svg :as svg]
            [patterns.utils :as utils]))

(defn points
  [xn yn {:keys [width height]}]
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
       (reduce (fn [accum [item pairs]]
                 (concat accum
                         (if (seq pairs)
                           (map (partial cons [head item])
                                pairs)
                           [[[head item]]])))
               [])
       vec))

(defn- line-sets
  [points]
  (for [pairs (point-pairs points)]
    (for [[start end] pairs]
      {:start start
       :end end})))

(defn swatches
  [xn yn & [{:keys [grid-size line-fn style]
             :or {line-fn svg/line
                  style "path {fill:none;stroke:rgb(255,0,0);stroke-width:2;}"
                  grid-size 10}}]]
  (let [dimension {:width (* (inc xn) grid-size)
                   :height (* (inc yn) grid-size)}]
    (vec
      (for [line-set (line-sets (points xn yn dimension))]
        (utils/veccat
          [:svg
           dimension
           [:defs {}
            [:style {} style]]]
          (map (partial line-fn dimension)
               line-set))))))
