(ns patterns.examples.2019-19
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [patterns.utils.svg.color :as color]
            [patterns.transform :as transform]
            [patterns.tile :as tile]
            [patterns.pipes :as pipes]
            [patterns.examples.2019-13 :refer [->int vector-field]]
            [taoensso.tufte :as trace]
            [patterns.utils.svg.polygon :as polygon]
            [patterns.utils.svg :as svg]
            [clojure.string :as str]
            [patterns.shatter :as shatter]
            [patterns.examples :as e]
            [patterns.utils :as utils]
            [patterns.utils.layout.align :as align]))



(def chart-height 80)
(def chart-centerline 40)
(def charts-count 10)

(defn csv-data->maps
  [csv-data]
  (map zipmap
       (->> (first csv-data)                                ;; First row is the header
            (map keyword)                                   ;; Drop if you want string keys instead
            repeat)
       (rest csv-data)))

(defn add-tavg
  [{:keys [TMAX TMIN TAVG] :as report}]
  (cond
    TAVG report
    (and TMIN TMAX) (assoc report
                      :TAVG (float (/ (+ TMIN TMAX)
                                      2)))
    :else report))

(defn add-prcp
  [{:keys [PRCP SNOW] :as report}]
  (cond
    (and PRCP (pos? PRCP)) report
    (and SNOW (pos? SNOW)) (assoc report
                             :PRCP (float (* (/ 1 13)
                                             SNOW)))
    :else report))

(defn ->nullable-float
  [x]
  (when (and (string? x)
             (seq x))
    (Float/parseFloat x)))

(defn updates
  [report]
  (-> report
      (update :TMAX ->nullable-float)
      (update :TMIN ->nullable-float)
      (update :TAVG ->nullable-float)
      (update :PRCP ->nullable-float)
      (update :SNOW ->nullable-float)))

(defn data
  []
  (->> (concat
         (with-open [reader (io/reader (io/resource "mke-temps/0.csv"))]
           (doall
             (csv-data->maps
               (csv/read-csv reader))))
         (with-open [reader (io/reader (io/resource "mke-temps/01.csv"))]
           (doall
             (csv-data->maps
               (csv/read-csv reader))))
         (with-open [reader (io/reader (io/resource "mke-temps/1.csv"))]
           (doall
             (csv-data->maps
               (csv/read-csv reader)))))
       (map updates)
       (map add-tavg)
       (map add-prcp)
       (filter :TAVG)
       (filter :PRCP)
       (sort-by :DATE)))

(defn chart-temps
  [fill temps]
  (utils/veccat
    [:svg
     {:height chart-height
      :width (count temps)}
     [:defs {}]]
    (map-indexed
      (fn [idx {:keys [TAVG]}]
        [:circle {:fill (color/map-> fill)
                  :cx idx :cy (- chart-centerline TAVG) :r 1}])
      temps)))

(defn gradient->color
  [global-max gradient n]
  (let [raw-idx (->int
                  (* (/ n
                        global-max)
                     (count gradient)))
        idx (cond
              (neg? raw-idx) 0
              (<= (count gradient) raw-idx) (dec (count gradient))
              :else raw-idx)]
    (color/map->
      (nth gradient
           idx))))

(defn chart-precips
  [->color precips]
  (utils/veccat
    [:svg
     {:height chart-height
      :width (count precips)}
     [:defs {}]]
    (map-indexed
      (fn [idx {:keys [PRCP]}]
        [:rect {:x idx :y 0
                :height chart-height
                :width 1
                :fill (->color PRCP)}])
      precips)))

(defn chart
  [temp-fill [precip-gradient-start precip-gradient-end] global-max-precip
   weather-report]
  (align/center [(chart-precips
                   (partial gradient->color
                            global-max-precip
                            (color/->gradient precip-gradient-start
                                              precip-gradient-end
                                              (count weather-report)))
                   weather-report)
                 (chart-temps temp-fill weather-report)]))

(defn charts
  [temp-fill [precip-gradient-start precip-gradient-end]
   weather-report]
  (let [global-max-precip (apply max (map :PRCP weather-report))]
    (map (partial chart
                  temp-fill
                  [precip-gradient-start precip-gradient-end]
                  global-max-precip)
         (partition (->int (/ (count weather-report)
                              charts-count))
                    weather-report))))

(defn tiled
  [chart-srcs]
  (tile/grid (map (partial e/scale {:width e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                                    :height (->int (/ e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT charts-count))})
                  chart-srcs)
             1 charts-count))

(defn gen
  []
  (let [full-data (data)
        weather-reports (partition (->int (/ (count full-data)
                                             7))
                                   full-data)
        temp-fill {:r 241 :g 196 :b 15}
        gradient-start {:r 133 :g 193 :b 233}
        gradient-end {:r 174 :g 250 :b 255}
        gen-fn (fn [idx_1_based year month day weather-report]
                 (e/render [year month day]
                           (tiled (charts temp-fill
                                          [gradient-start gradient-end]
                                          weather-report))
                           (e/format-w-newlines
                             [["Playing with weather data this week for our hometown @Milwaukee!"]
                              ["Each rectangular background is a day between %s and %s."
                               "The lighter the blue, the heavier the precipitation on that day, the lighter the shade of blue."
                               "The yellow dashes represent the temperature on that day."]
                              ["Data has been pulled from @NOAA."]]
                             (-> weather-report
                                 first
                                 :DATE)
                             (-> weather-report
                                 last
                                 :DATE))
                           {:recursive? false}))]
    (doseq [[[idx_1_based year month day] weather-report] (map list
                                                               (e/indexed-days-of-week (e/week->date 2019 19))
                                                               weather-reports)]
      (gen-fn idx_1_based year month day weather-report))))

(comment
  (trace/profile {} (gen)))

