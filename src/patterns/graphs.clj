(ns patterns.graphs
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clustering.average.simple :as clust.average]
            [clustering.core.k-means :as k-means]
            [clustering.distance.euclidean :as clust.euc-dist]
            [inkspot.color :as ink.color]
            [inkspot.color-chart :as ink.cc]
            [inkspot.color-chart.lindsay :as ink.lindsay]
            [inkspot.color-chart.x11 :as ink.x11]
            [mikera.image.colours :as img.colors]
            [mikera.image.core :as img]
            [patterns.core :as patterns]
            [patterns.hiccup :as hiccup]
            [patterns.utils :as utils]
            [patterns.utils.svg :as svg])
  (:import [java.awt Color]
           [java.io File]))

(set! *warn-on-reflection* true)

(defn k-means-pixels-mapping
  "Returns {color: k-color, ...} map"
  [k pixels]
  (println "k-means-pixels-mapping ::: start")
  (let [pixels (->> pixels
                    (remove (fn [argb]
                              (zero? (:a argb))))
                    (map (fn [{:keys [a r g b]}]
                           [a r g b])))]
    (println "k-means-pixels-mapping ::: clustering")
    (->> (k-means/centroids clust.euc-dist/distance
                            clust.average/average
                            pixels
                            (k-means/init-means k pixels)
                            1.0)
         (k-means/classify clust.euc-dist/distance pixels)
         (into {}
               (mapcat (fn [[[k-a k-r k-g k-b] v]]
                         (map (fn [[v-a v-r v-g v-b]]
                                [{:a v-a
                                  :r v-r
                                  :g v-g
                                  :b v-b}
                                 {:a (float (/ (Math/round ^Float (* k-a 100))
                                               100))
                                  :r (int k-r)
                                  :g (int k-g)
                                  :b (int k-b)}])
                              v)))))))

(defn k-means-pixels-data
  [k pixels]
  (println "k-means-pixels-data ::: start")
  (let [pixels (->> pixels
                    (remove (fn [argb]
                              (zero? (:a argb))))
                    (map (fn [{:keys [a r g b]}]
                           [a r g b])))
        pixel-count (count pixels)
        _ (println "k-means-pixels-data ::: clustering")
        groups (->>
                 (k-means/centroids clust.euc-dist/distance
                                    clust.average/average
                                    pixels
                                    (k-means/init-means k pixels)
                                    1.0)
                 (k-means/classify clust.euc-dist/distance pixels)
                 (map (fn [[[a r g b] v]]
                        [{:a (float (/ (Math/round ^Float (* a 100))
                                       100))
                          :r (int r)
                          :g (int g)
                          :b (int b)} (count v)])))]
    (println "k-means-pixels-data ::: returning")
    (into {}
          (map (fn [[k v]]
                 [k (float (/ v pixel-count))]))
          groups)))

(defn k-means-png-data
  [filename k]
  (->> (img/get-pixels (img/load-image filename))
       (map img.colors/color)
       (map (fn [^Color c]
              {:a (float (/ (.getAlpha c)
                            255))
               :r (.getRed c)
               :g (.getGreen c)
               :b (.getBlue c)}))
       (k-means-pixels-data k)))

(defn histogram-png-data
  [filename]
  (let [pixels (->> (img/get-pixels (img/load-image filename))
                    (map img.colors/color)
                    (remove (fn [^Color c]
                              (zero? (.getAlpha c)))))
        pixel-count (count pixels)]
    (into {}
          (map (fn [[k v]]
                 [k (float (/ v pixel-count))]))
          ;; We use our own version of frequencies here for performance
          (persistent!
            (reduce (fn [counts ^Color c]
                      (let [argb {:a (float (/ (.getAlpha c)
                                               255))
                                  :r (.getRed c)
                                  :g (.getGreen c)
                                  :b (.getBlue c)}]
                        (assoc! counts
                                argb (inc (get counts argb 0)))))
                    (transient {})
                    pixels)))))

(defn histogram-data
  "Given an svg src, return a histogram of the argb values present.

  Returns:
  {{:a [0-1]
    :r [0-255]
    :g [0-255]
    :b [0-255]}: (0-1]}"
  [src]
  (let [tmp-resource-name (patterns/render
                            (patterns/tmp-resource)
                            src
                            :png)
        h (histogram-png-data tmp-resource-name)]
    (io/delete-file tmp-resource-name)
    h))

(defn bar
  "bar-data [[argb: float%] ...] (can be map, etc.)"
  [{:keys [height width] :as dimensions} bar-data]
  (let [bars (->> bar-data
                  (map (fn [[k percent]]
                         [k (Math/round ^Float (* height percent))]))
                  (remove (comp zero? second))
                  (take-last width))
        num-colours (count bars)
        bar-width (-> (/ width
                         num-colours)
                      (Math/ceil)
                      int)]
    (utils/veccat
      [:svg
       dimensions
       [:defs {}]]
      (map-indexed (fn [idx [{:keys [a r g b]} bar-height]]
                     [:rect {:style (format "fill:rgb(%s,%s,%s);fill-opacity:%s;"
                                            r g b a)
                             :x (- width
                                   (* (inc idx) bar-width))
                             :y (- height bar-height)
                             :height bar-height
                             :width bar-width}])
                   bars))))

(defn histogram
  [src]
  (bar (svg/dimensions src)
       (->> src
            histogram-data
            (sort-by second))))

(defn bar-overlay
  [src bar-src & [{:keys [opacity]
                   :or {opacity 0.5}}]]
  (let [src-id (gensym "g")
        bar-graph-id (gensym "g")]
    [:svg
     (svg/dimensions src)
     [:defs
      (svg/->def src src-id)
      (svg/->def bar-src bar-graph-id)]
     [:g {:opacity opacity}
      (svg/use src-id {:x 0 :y 0})]
     (svg/use bar-graph-id {:x 0 :y 0})]))

(defn histogram-overlay
  [src & [{:keys [opacity]
           :or {opacity 0.5}}]]
  (bar-overlay src (histogram src) :opacity opacity))

(defn image->palette
  [src]
  (let [[filename prefix] (if (and (seqable? src)
                                   (= :svg (first src)))
                            [(patterns/render src :png) ""]
                            [src (str/replace (cond
                                                (instance? File src) (.getName ^File src)
                                                (string? src) src)
                                              #".png"
                                              "")])]
    (->> (histogram-png-data filename)
         (sort-by second)
         reverse
         (map-indexed (fn [idx [argb]]
                        [(keyword
                           (str prefix
                                "-" idx))
                         argb])))))

(comment
  {:web-safe-colors (map ink.color/coerce ink.cc/web-safe-colors)
   :lindsay (map ink.color/coerce (vals ink.lindsay/swatch))
   :x11 (map ink.color/coerce (vals ink.x11/swatch))}

  (+ (count ink.cc/web-safe-colors)
     (count ink.lindsay/swatch)
     (count ink.x11/swatch))

  (let [k 10
        groups (->> (io/file "./resources/swatches/png/pokemon/")
                    file-seq
                    rest
                    (mapcat (fn [^java.io.File f]
                              (image->palette f)))
                    (map second)
                    (k-means-pixels-data k))]
    (patterns/render
      "./poke.k-means"
      (patterns.tile/grid
        (map (fn [[{:keys [r g b]}]]
               [:svg
                {:width 10 :height 10}
                [:rect {:style (format "fill:rgb(%s,%s,%s);"
                                       r g b)
                        :x 0
                        :y 0
                        :height 10
                        :width 10}]])
             groups)
        k k)
      :png))
  (patterns/render
    "./poke.k-means"
    (patterns.tile/grid
      (->> (io/file "./resources/swatches/png/pokemon/")
           file-seq
           rest
           (mapcat (fn [^java.io.File f]
                     (image->palette f)))

           (map (fn [[_k {:keys [r g b]}]]
                  [:svg
                   {:width 10 :height 10}
                   [:rect {:style (format "fill:rgb(%s,%s,%s);"
                                          r g b)
                           :x 0
                           :y 0
                           :height 10
                           :width 10}]])))
      607 14)
    :png)

  (patterns/render
    "./bar-histogram"
    (histogram-overlay

      (hiccup/file->hiccup "/Users/alexandermann/git/patterns/resources/pipes.svg"))
    :png))
