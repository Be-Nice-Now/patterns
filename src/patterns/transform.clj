(ns patterns.transform
  (:refer-clojure :exclude [shuffle])
  (:require [inkspot.color :as ink.color]
            [mikera.image.core :as img]
            [patterns.core :as patterns]
            [patterns.graphs :as graphs]
            [patterns.tile :as tile]
            [patterns.utils.svg :as svg]
            [clojure.java.io :as io]
            [mikera.image.colours :as img.colors])
  (:import [java.awt Color]))

(set! *warn-on-reflection* true)

(defn bin-idx
  [weights]
  (let [intervals (vec (reductions + weights))
        high-start (dec (count intervals))]
    (fn [n]
      (loop [low 0
             high high-start]
        (if (== high low)
          high
          (let [m (unchecked-add low (bit-shift-right (unchecked-subtract high low) 1))]
            (if (< (intervals m) n)
              (recur (unchecked-inc m) high)
              (recur low m))))))))

(defn shuffle
  [src]
  (let [hist (vec (graphs/histogram-data src))
        _ (println "hist generated")
        hist-pre-colours (map first
                              hist)
        hist-colours (mapv (comp (fn [^Color c]
                                   (.getRGB c))
                                 ink.color/coerce)
                           hist-pre-colours)
        colour-gen-gen (bin-idx (map second hist))
        colour-gen (fn []
                     (hist-colours (colour-gen-gen (rand))))
        {:keys [height width]} (svg/dimensions src)
        swatch-img (img/new-image height width)
        pixels (img/get-pixels swatch-img)
        swatch (patterns/tmp-resource)]
    (println "setting pixels")
    (dotimes [i (* height width)]
      (aset pixels i
            ^long (colour-gen)))
    (println "writing swatch")
    (img/set-pixels swatch-img pixels)
    (img/write swatch-img
               swatch
               "png"
               :quality 1.0)
    swatch))

(defn tile-shuffle
  [src {:keys [height width]}]
  (let [hist (vec (graphs/histogram-data src))
        _ (println "hist generated")
        hist-colours (mapv first
                           hist)
        tile-dims (-> hist-colours
                      count
                      float
                      Math/sqrt
                      Math/ceil
                      int)
        swatch-height (int (Math/ceil (float (/ height tile-dims))))
        swatch-width (int (Math/ceil (float (/ width tile-dims))))
        _ (println (format "dimensions: total colours: %s, tile w h: %s, swatch w: %s, swatch h: %s"
                           (count hist-colours)
                           tile-dims
                           swatch-width
                           swatch-height))
        colour-gen-gen (bin-idx (map second hist))
        colour-gen (fn []
                     (let [{:keys [r g b]} (hist-colours (colour-gen-gen (rand)))]
                       [:svg {:height swatch-height :width swatch-width}
                        [:defs {}]
                        [:rect {:height swatch-height :width swatch-width
                                :style (format "fill:rgb(%s,%s,%s);"
                                               r g b)}]]))]
    (println "writing swatch")
    (tile/grid
      (repeatedly (* tile-dims tile-dims)
                  colour-gen)
      tile-dims tile-dims)))

(defn shuffle-k-means
  [src k]
  (let [png (patterns/render (patterns/tmp-resource)
                             src :png)
        _ (println "tmp rendered")
        hist (vec (graphs/k-means-png-data png k))
        _ (println "hist generated")
        hist-pre-colours (map first
                              hist)
        hist-colours (mapv (comp (fn [^Color c]
                                   (.getRGB c))
                                 ink.color/coerce)
                           hist-pre-colours)
        colour-gen-gen (bin-idx (map second hist))
        colour-gen (fn []
                     (hist-colours (colour-gen-gen (rand))))
        {:keys [height width]} (svg/dimensions src)
        swatch-img (img/new-image height width)
        pixels (img/get-pixels swatch-img)
        swatch (patterns/tmp-resource)]
    (io/delete-file png)
    (println "setting pixels")
    (dotimes [i (* height width)]
      (aset pixels i
            ^long (colour-gen)))
    (println "writing swatch")
    (img/set-pixels swatch-img pixels)
    (img/write swatch-img
               swatch
               "png"
               :quality 1.0)
    swatch))

(defn tile-shuffle-k-means
  [src k {:keys [height width]}]
  (let [png (patterns/render (patterns/tmp-resource)
                             src :png)
        _ (println "tmp rendered")
        hist (vec (graphs/k-means-png-data png k))
        _ (println "hist generated")
        hist-colours (mapv first
                           hist)
        tile-dims (-> hist-colours
                      count
                      float
                      Math/sqrt
                      Math/ceil
                      int)
        swatch-height (int (Math/ceil (float (/ height tile-dims))))
        swatch-width (int (Math/ceil (float (/ width tile-dims))))
        _ (println (format "dimensions: total colours: %s, tile w h: %s, swatch w: %s, swatch h: %s"
                           (count hist-colours)
                           tile-dims
                           swatch-width
                           swatch-height))
        colour-gen-gen (bin-idx (map second hist))
        colour-gen (fn []
                     (let [{:keys [r g b]} (hist-colours (colour-gen-gen (rand)))]
                       [:svg {:height swatch-height :width swatch-width}
                        [:defs {}]
                        [:rect {:height swatch-height :width swatch-width
                                :style (format "fill:rgb(%s,%s,%s);"
                                               r g b)}]]))]
    (io/delete-file png)
    (println "writing swatch")
    (tile/grid
      (repeatedly (* tile-dims tile-dims)
                  colour-gen)
      tile-dims tile-dims)))

(defn rasterize
  [src k]
  (let [png (patterns/render (patterns/tmp-resource)
                             src :png)
        _ (println "tmp rendered")
        src-pixels (->> (img/get-pixels (img/load-image png))
                        (map img.colors/color)
                        (mapv (fn [^Color c]
                                {:a (float (/ (.getAlpha c)
                                              255))
                                 :r (.getRed c)
                                 :g (.getGreen c)
                                 :b (.getBlue c)})))
        mapping (into {}
                      (map (fn [[k v]]
                             [k (.getRGB ^Color (ink.color/coerce v))]))
                      (graphs/k-means-pixels-mapping k src-pixels))
        _ (println "mapping generated:  " (type mapping) (take 5 mapping) (count mapping))
        {:keys [height width]} (svg/dimensions src)
        swatch-img (img/new-image height width)
        pixels (img/get-pixels swatch-img)
        swatch (patterns/tmp-resource)]
    (io/delete-file png)
    (println "setting pixels")
    (dotimes [i (* height width)]
      (aset pixels i
            ^long (get mapping (src-pixels i))))
    (println "writing swatch")
    (img/set-pixels swatch-img pixels)
    (img/write swatch-img
               swatch
               "png"
               :quality 1.0)
    swatch))

(comment
  (shuffle-k-means
    [:svg {:height 100 :width 100}
     {}
     [:defs {}]
     [:image {:xlink:href "/tmp/DSC06765.png"
              :height 100 :width 100
              :x 0 :y 0}]]))
