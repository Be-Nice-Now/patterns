(ns patterns.transform
  (:refer-clojure :exclude [shuffle])
  (:require [inkspot.color :as ink.color]
            [mikera.image.core :as img]
            [patterns.core :as patterns]
            [patterns.graphs :as graphs]
            [patterns.tile :as tile]
            [patterns.utils.svg :as svg]
            [clojure.java.io :as io]
            [mikera.image.colours :as img.colors]
            [taoensso.tufte :as trace]
            [taoensso.timbre :as log])
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

(trace/defnp ^:private write
  [{:keys [height width]} colour-gen]
  (let [swatch-img (img/new-image width height)
        pixels (img/get-pixels swatch-img)
        swatch (patterns/tmp-resource)]
    (dotimes [i (* height width)]
      (aset pixels i
            (trace/p :colour-gen
              ^long (colour-gen i))))
    (trace/p :set-pixels
      (img/set-pixels swatch-img pixels))
    (trace/p :write-img
      (img/write swatch-img
                 swatch
                 "png"
                 :quality 1.0))
    swatch))

(defn shuffle
  [src]
  (let [hist (vec (graphs/histogram-data src))
        hist-pre-colours (map first
                              hist)
        hist-colours (mapv (comp (fn [^Color c]
                                   (.getRGB c))
                                 ink.color/coerce)
                           hist-pre-colours)
        colour-gen-gen (bin-idx (map second hist))
        colour-gen (fn [& _args]
                     (hist-colours (colour-gen-gen (rand))))]
    (write (svg/dimensions src)
           colour-gen)))

(defn tile-shuffle
  [src {:keys [height width]}]
  (let [hist (vec (graphs/histogram-data src))
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
        colour-gen-gen (bin-idx (map second hist))
        colour-gen (fn [& _args]
                     (let [{:keys [r g b]} (hist-colours (colour-gen-gen (rand)))]
                       [:svg {:height swatch-height :width swatch-width}
                        [:defs {}]
                        [:rect {:height swatch-height :width swatch-width
                                :style (format "fill:rgb(%s,%s,%s);"
                                               r g b)}]]))]
    (log/debugf "dimensions: total colours: %s, tile w h: %s, swatch w: %s, swatch h: %s"
                (count hist-colours)
                tile-dims
                swatch-width
                swatch-height)
    (tile/grid
      (repeatedly (* tile-dims tile-dims)
                  colour-gen)
      tile-dims tile-dims)))

(defn shuffle-k-means
  [src k]
  (let [png (patterns/render (patterns/tmp-resource)
                             src :png)
        hist (vec (graphs/k-means-png-data png k))
        hist-pre-colours (map first
                              hist)
        hist-colours (mapv (comp (fn [^Color c]
                                   (.getRGB c))
                                 ink.color/coerce)
                           hist-pre-colours)
        colour-gen-gen (bin-idx (map second hist))
        colour-gen (fn []
                     (hist-colours (colour-gen-gen (rand))))]
    (io/delete-file png)
    (write (svg/dimensions src)
           colour-gen)))

(defn tile-shuffle-k-means
  [src k {:keys [height width]}]
  (let [png (patterns/render (patterns/tmp-resource)
                             src :png)
        hist (vec (graphs/k-means-png-data png k))
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
        colour-gen-gen (bin-idx (map second hist))
        colour-gen (fn []
                     (let [{:keys [r g b]} (hist-colours (colour-gen-gen (rand)))]
                       [:svg {:height swatch-height :width swatch-width}
                        [:defs {}]
                        [:rect {:height swatch-height :width swatch-width
                                :style (format "fill:rgb(%s,%s,%s);"
                                               r g b)}]]))]
    (log/debugf "dimensions: total colours: %s, tile w h: %s, swatch w: %s, swatch h: %s"
                (count hist-colours)
                tile-dims
                swatch-width
                swatch-height)
    (io/delete-file png)
    (tile/grid
      (repeatedly (* tile-dims tile-dims)
                  colour-gen)
      tile-dims tile-dims)))

(defn rasterize
  [src k]
  (let [png (patterns/render (patterns/tmp-resource)
                             src :png)
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
                      (graphs/k-means-pixels-mapping k src-pixels))]
    (io/delete-file png)
    (write (svg/dimensions src)
           #(get mapping (src-pixels %)))))

(comment
  (shuffle-k-means
    [:svg {:height 100 :width 100}
     {}
     [:defs {}]
     [:image {:xlink:href "/tmp/DSC06765.png"
              :height 100 :width 100
              :x 0 :y 0}]]))
