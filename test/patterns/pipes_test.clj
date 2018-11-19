(ns patterns.pipes-test
  (:require [clojure.test :refer :all]
            [patterns.core :as patterns]
            [patterns.pipes :as pipes]
            [patterns.tile :as tile]
            [patterns.utils.paths :as utils.paths]
            [patterns.utils.svg :as svg]
            [patterns.test-utils :as t.utils]))

(deftest pipes-straight-lines
  (let [hiccup-svg (tile/grid
                     (pipes/swatches
                       1 1)
                     3 3)]
    #_(patterns/render
        "./doc/examples/pipes-straight.svg"
        hiccup-svg)
    (is (-> hiccup-svg
            t.utils/hiccup-validation-errors
            empty?))))

(deftest pipes-curves--pi
  (let [hiccup-svg (tile/grid
                     [(nth (pipes/swatches
                             1 1
                             {:line-fn (fn [& args]
                                         [:g {}
                                          [:g {:id "backfill"}
                                           (apply svg/quadratic args)]
                                          [:g {:id "midfill"}
                                           (apply svg/quadratic args)]
                                          (apply svg/quadratic args)])
                              :style "path {fill:none;stroke:rgb(255,0,0);stroke-width:1;} #midfill path {fill:none;stroke:rgb(255,255,255);stroke-width:14;} #backfill path {fill:none;stroke:rgb(255,0,0);stroke-width:15;}"})
                           2)]
                     4 4
                     {:transform-fn tile/pi-rotate})]
    #_(patterns/render
      "./doc/examples/pipes-curves--pi.svg"
      hiccup-svg)
    (is (-> hiccup-svg
            t.utils/hiccup-validation-errors
            empty?))))

(deftest pipes-complex
  (let [hiccup-svg (tile/grid
                     (pipes/swatches
                       3 3
                       {:line-fn (partial utils.paths/straight-line-fn 5)
                        :style "path {fill:none;stroke:rgb(255,0,0);stroke-width:1;} #midfill path {fill:none;stroke:rgb(255,255,255);stroke-width:3;} #backfill path {fill:none;stroke:rgb(255,0,0);stroke-width:5;}"})
                     7 7)]
    #_(patterns/render
        "./doc/examples/pipes-complex.svg"
        hiccup-svg)
    (is (-> hiccup-svg
            t.utils/hiccup-validation-errors
            empty?))))

(deftest pipes-complex-curved
  (let [hiccup-svg (tile/grid
                     (pipes/swatches
                       3 3
                       {:line-fn (partial utils.paths/curved-line-fn 5)
                        :style "path {fill:none;stroke:rgb(255,0,0);stroke-width:1;stroke-linejoin:round;} #midfill path {fill:none;stroke:rgb(255,255,255);stroke-width:3;stroke-linejoin:round;} #backfill path {fill:none;stroke:rgb(255,0,0);stroke-width:5;stroke-linejoin:round;}"})
                     7 7)]
    #_(patterns/render
        "./doc/examples/pipes-complex-curved.svg"
        hiccup-svg)
    (is (-> hiccup-svg
            t.utils/hiccup-validation-errors
            empty?))))

(deftest pipes-complex-turned
  (let [hiccup-svg (tile/grid
                     (pipes/swatches
                       3 3
                       {:line-fn (partial utils.paths/turned-line-fn 5)
                        :style "path {fill:none;stroke:rgb(255,0,0);stroke-width:1;stroke-linejoin:round;} #midfill path {fill:none;stroke:rgb(255,255,255);stroke-width:3;stroke-linejoin:round;} #backfill path {fill:none;stroke:rgb(255,0,0);stroke-width:5;stroke-linejoin:round;}"})
                     7 7)]
    #_(patterns/render
        "./doc/examples/pipes-complex-turned.svg"
        hiccup-svg)
    (is (-> hiccup-svg
            t.utils/hiccup-validation-errors
            empty?))))

(deftest pipes-complex-single-bend
  (let [hiccup-svg (tile/grid
                     (pipes/swatches
                       3 3
                       {:line-fn (partial utils.paths/single-bend-line-fn 5)
                        :style "path {fill:none;stroke:rgb(255,0,0);stroke-width:1;stroke-linejoin:round;} #midfill path {fill:none;stroke:rgb(255,255,255);stroke-width:3;stroke-linejoin:round;} #backfill path {fill:none;stroke:rgb(255,0,0);stroke-width:5;stroke-linejoin:round;}"})
                     7 7)]
    #_(patterns/render
        "./doc/examples/pipes-complex-single-bend.svg"
        hiccup-svg)
    (is (-> hiccup-svg
            t.utils/hiccup-validation-errors
            empty?))))