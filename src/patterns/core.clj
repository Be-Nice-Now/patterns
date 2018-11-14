(ns patterns.core
  (:require [hiccup.core :as html]
            [patterns.hiccup :as hiccup]
            [patterns.pipes :as pipes]
            [patterns.tile :as tile]
            [patterns.utils.svg :as svg]))

(defn render
  [filename src]
  (->> src
       svg/to-svg-doc
       html/html
       (spit filename)))

(def pi-seq (vec (map #(Integer/valueOf (str %))
                      (clojure.string/replace
                        (str
                          (Math/PI))
                        #"\." ""))))

(defn pi-rotate
  [& args]
  (apply tile/transform-rotate
         (fn [_src [x y] & _args]
           (mod (* 90
                   (get pi-seq
                        (mod (+ x y)
                             (count pi-seq))))
                360))
         args))

(comment
  (defn resource->hiccup
    [n]
    (-> n
        clojure.java.io/resource
        slurp
        hiccup/svg->hiccup))

  (do
    (render
      "./doc/examples/pipes-tiled_3_3.svg"
      (-> "pipes.svg"
          resource->hiccup
          vector
          (tile/grid 3 3)))

    (render
      "./doc/examples/tiled-alternating_10_3.svg"
      (tile/grid
        [(resource->hiccup "pipes.svg")
         (resource->hiccup "bnn-logo.svg")]
        10 3))

    (render
      "./doc/examples/pipes-rotated_20_20.svg"
      (-> "pipes.svg"
          resource->hiccup
          vector
          (tile/grid
            20 20
            {:transform-fn tile/transform-rotate})))

    (render
      "./doc/examples/bnn-logo-rotated_11_7.svg"
      (-> "bnn-logo.svg"
          resource->hiccup
          vector
          (tile/grid
            11 7
            {:transform-fn tile/transform-rotate})))

    (render
      "./doc/examples/pipes-straight.svg"
      (tile/grid
        (pipes/swatches
          1 1)
        3 3))

    (render
      "./doc/examples/pipes-rotated-pi_8_8.svg"
      (-> "pipes.svg"
          resource->hiccup
          vector
          (tile/grid
            8 8
            {:transform-fn pi-rotate})))

    (render
      "./doc/examples/pipes-curves-multi-strokes.svg"
      (tile/grid
        [(tile/grid
           (pipes/swatches
             1 1
             {:line-fn (fn [& args]
                         [:g {}
                          [:g {:id "backfill"}
                           (apply svg/quadratic args)]
                          [:g {:id "midfill"}
                           (apply svg/quadratic args)]
                          (apply svg/quadratic args)])
              :style "path {fill:none;stroke:rgb(255,0,0);stroke-width:1;} #midfill path {fill:none;stroke:rgb(255,255,255);stroke-width:14;} #backfill path {fill:none;stroke:rgb(255,0,0);stroke-width:15;}"})
           5 5
           {:transform-fn pi-rotate})]
        3 3))

    (render
      "./doc/examples/pipes-complex.svg"
      (tile/grid
        (pipes/swatches
          3 3)
        7 7))))
