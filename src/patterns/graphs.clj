(ns patterns.graphs
  (:require [clojure.java.io :as io]
            [mikera.image.core :as img]
            [mikera.image.colours :as img.colors]
            [patterns.hiccup :as hiccup]
            [patterns.utils :as utils]
            [patterns.utils.svg :as svg])
  (:import [java.awt Color]))

(set! *warn-on-reflection* true)

(defn histogram-data
  "Given an svg src, return a histogram of the argb values present.

  Returns:
  {{:a [0-1]
    :r [0-255]
    :g [0-255]
    :b [0-255]}: (0-100]}"
  [src]
  (let [tmp-resource-name (patterns.core/render
                            (patterns.core/tmp-resource)
                            src
                            :png)
        pixels (img/get-pixels (img/load-image tmp-resource-name))
        pixel-count (count pixels)]
    (io/delete-file tmp-resource-name)
    (into {}
          (map (fn [[k v]]
                 [k (float (/ v pixel-count))]))
          ;; We use our own version of frequencies here for performance
          (persistent!
            (reduce (fn [counts x]
                      (let [^Color c (img.colors/color x)
                            argb {:a (float (/ (.getAlpha c)
                                               255))
                                  :r (.getRed c)
                                  :g (.getGreen c)
                                  :b (.getBlue c)}]
                        (assoc! counts
                                argb (inc (get counts argb 0)))))
                    (transient {})
                    pixels)))))

(defn histogram
  [src]
  (let [{:keys [height width] :as dimensions} (svg/dimensions src)
        hist (->> src
                  histogram-data
                  (sort-by second)
                  (take-last width)
                  (map (fn [[k percent]]
                         [k (Math/round ^Float (* height percent))]))
                  (remove (comp zero? second)))
        num-colours (count hist)
        bar-width (int (/ width
                          num-colours))]
    (utils/veccat
      [:svg
       dimensions]
      (map-indexed (fn [idx [{:keys [a r g b]} bar-height]]
                     [:rect {:style (format "fill:rgb(%s,%s,%s);fill-opacity:%s;"
                                            r g b a)
                             :x (* idx bar-width)
                             :y (- height bar-height)
                             :height bar-height
                             :width bar-width}])
                   hist))))

(defn histogram-overlay
  [src & [{:keys [opacity]
           :or {opacity 0.5}}]]
  (let [src-id (gensym "h")
        histogram-id (gensym "h")]
    [:svg
     (svg/dimensions src)
     [:defs
      (svg/->def src src-id)
      (svg/->def (histogram src) histogram-id)]
     [:g {:opacity opacity}
      (svg/use src-id {:x 0 :y 0})]
     (svg/use histogram-id {:x 0 :y 0})]))

(comment
  (patterns.core/render
    "./bar-histogram"
    (histogram-overlay
      (hiccup/file->hiccup "/Users/alexandermann/git/patterns/doc/social_media/save/2018-11-25--save.svg"))
    :png))
