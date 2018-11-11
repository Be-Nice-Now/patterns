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
  (render
    "./pipes-tiled_3_3.svg"
    (-> "pipes.svg"
        clojure.java.io/resource
        slurp
        hiccup/svg->hiccup
        (tile/grid 3 3)))

  (render
    "./pipes-rotated_20_20.svg"
    (-> "pipes.svg"
        clojure.java.io/resource
        slurp
        hiccup/svg->hiccup
        (tile/grid
          20 20
          {:transform-fn tile/rotate})))

  (render
    "./bnn-logo-rotated_11_7.svg"
    (-> "bnn-logo.svg"
        clojure.java.io/resource
        slurp
        hiccup/svg->hiccup
        (tile/grid
          11 7
          {:transform-fn tile/rotate})))

  (def pi-seq (vec (map #(Integer/valueOf (str %))
                        (clojure.string/replace
                          (str
                            (Math/PI))
                          #"\." ""))))

  (render
    "./pipes-rotated-pi_8_8.svg"
    (-> "pipes.svg"
        clojure.java.io/resource
        slurp
        hiccup/svg->hiccup
        (tile/grid
          8 8
          {:transform-fn (partial tile/rotate
                                  (fn [_src [x y] & _args]
                                    (mod (* 90
                                            (get pi-seq
                                                 (+ x y)))
                                         360)))}))))
