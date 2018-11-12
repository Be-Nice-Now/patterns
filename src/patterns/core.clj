(ns patterns.core
  (:require [hiccup.core :as html]
            [patterns.hiccup :as hiccup]
            [patterns.tile :as tile]
            [patterns.utils.svg :as svg]))

(defn render
  [filename src]
  (->> src
       svg/to-svg-doc
       html/html
       (spit filename)))

(comment
  (defn resource->hiccup
    [n]
    (-> n
        clojure.java.io/resource
        slurp
        hiccup/svg->hiccup))

  (resource->hiccup
    "pipes.svg")

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

  (def pi-seq (vec (map #(Integer/valueOf (str %))
                        (clojure.string/replace
                          (str
                            (Math/PI))
                          #"\." ""))))

  (render
    "./doc/examples/pipes-rotated-pi_8_8.svg"
    (-> "pipes.svg"
        resource->hiccup
        vector
        (tile/grid
          8 8
          {:transform-fn (partial tile/transform-rotate
                                  (fn [_src [x y] & _args]
                                    (mod (* 90
                                            (get pi-seq
                                                 (+ x y)))
                                         360)))}))))
