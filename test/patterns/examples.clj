(ns patterns.examples
  (:require [clojure.test :refer :all]
            [patterns.core :as patterns]
            [patterns.tile :as tile]
            [patterns.utils.svg :as svg]
            [patterns.pipes :as pipes]
            [patterns.utils.paths :as utils.paths]))

(defn instagram-2018-46
  []
  (doseq [[idx day] (map list
                         (range)
                         (range 19 (+ 19 7)))]
    (let [filename (format "./doc/social_media/2018-11-%s--0"
                           day)
          pipe-endpoints (inc (rand-int 3))
          line-fn (if (= 1 pipe-endpoints)
                    svg/quadratic
                    (partial utils.paths/single-bend-line-fn (* (inc idx) (inc idx))))
          grid-size (int (/ 1080 day pipe-endpoints))
          {:keys [style path-fn]} (svg/multi-stroke (for [i (range (inc idx))]
                                                      (let [gray-tone (int (max 0
                                                                                (- 255
                                                                                   (* (inc i)
                                                                                      (/ 255 (inc idx))))))
                                                            line-width (int (* (inc i)
                                                                               (/ grid-size
                                                                                  (inc idx))))]
                                                        (println :lines gray-tone line-width)
                                                        {:width line-width
                                                         :colour {:r gray-tone
                                                                  :g gray-tone
                                                                  :b gray-tone}})))
          hiccup-svg (tile/grid
                       (pipes/swatches
                         pipe-endpoints pipe-endpoints
                         {:line-fn (fn [& args]
                                     (path-fn (apply line-fn args)))
                          :style style
                          :grid-size grid-size})
                       day day
                       {:transform-fn tile/transform-rotate})]
      (println :def idx day pipe-endpoints grid-size)
      (try
        (patterns/render
          filename
          hiccup-svg
          :png)
        (catch Error e
          (println e)
          (throw (ex-info "Error" {} e)))))))

(comment
  (instagram-2018-46))
