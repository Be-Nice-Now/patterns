(ns patterns.tile-test
  (:require [clojure.test :refer :all]
            [patterns.core :as patterns]
            [patterns.test-utils :as t.utils]
            [patterns.tile :as tile]))

(deftest pipes-tiled_3_3
  (let [hiccup-svg (-> "pipes.svg"
                       t.utils/resource->hiccup
                       vector
                       (tile/grid 3 3))]
    #_(patterns/render
        "./doc/examples/pipes-tiled_3_3.svg"
        hiccup-svg)
    (is (-> hiccup-svg
            t.utils/hiccup-validation-errors
            empty?))))

(deftest tiled-alternating_10_3
  (let [hiccup-svg (tile/grid
                     [(t.utils/resource->hiccup "pipes.svg")
                      (t.utils/resource->hiccup "bnn-logo.svg")]
                     10 3)]
    #_(patterns/render
        "./doc/examples/tiled-alternating_10_3.svg"
        hiccup-svg)
    (is (-> hiccup-svg
            t.utils/hiccup-validation-errors
            empty?))))

(deftest pipes-rotated_20_20
  (let [hiccup-svg (-> "pipes.svg"
                       t.utils/resource->hiccup
                       vector
                       (tile/grid
                         20 20
                         {:transform-fn tile/transform-rotate}))]
    #_(patterns/render
        "./doc/examples/pipes-rotated_20_20.svg"
        hiccup-svg)
    (is (-> hiccup-svg
            t.utils/hiccup-validation-errors
            empty?))))

(deftest bnn-logo-rotated_11_7
  (let [hiccup-svg (-> "bnn-logo.svg"
                       t.utils/resource->hiccup
                       vector
                       (tile/grid
                         11 7
                         {:transform-fn tile/transform-rotate}))]
    #_(patterns/render
        "./doc/examples/bnn-logo-rotated_11_7.svg"
        hiccup-svg)
    (is (-> hiccup-svg
            t.utils/hiccup-validation-errors
            empty?))))

(deftest pipes-rotated-pi_8_8
  (let [hiccup-svg (-> "pipes.svg"
                       t.utils/resource->hiccup
                       vector
                       (tile/grid 3 3))]
    #_(patterns/render
        "./doc/examples/tiled-alternating_10_3.svg"
        hiccup-svg)
    (is (-> hiccup-svg
            t.utils/hiccup-validation-errors
            empty?)))
  (patterns/render
    "./doc/examples/pipes-rotated-pi_8_8.svg"
    (-> "pipes.svg"
        t.utils/resource->hiccup
        vector
        (tile/grid
          8 8
          {:transform-fn tile/pi-rotate}))))
