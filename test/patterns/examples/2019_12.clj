(ns patterns.examples.2019-12
  (:require [patterns.examples :as e]
            [taoensso.tufte :as trace]
            [patterns.utils.svg.polygon :as polygon]
            [patterns.utils :as utils]
            [patterns.utils.svg.color :as color]
            [patterns.utils.svg :as svg]
            [patterns.utils.layout.align :as align]))

;; Whitesmoke is 245 245 245
(def ideal-gray 245)

(defn- ->graytone
  "Given a colour, determine the ideal graytone for it. ie, shift it so that it resides
  more in the gray colour space."
  [{:keys [a r g b]}]
  (let [diff (- ideal-gray
                (max r g b))]
    {:a a
     :r (+ r diff)
     :g (+ g diff)
     :b (+ b diff)}))

(defn background-swatch
  [colour]
  [:rect {:fill (color/map-> colour)
          :x 0 :y 0
          :width e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
          :height e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT}])

(defn triangle-swatch
  [n primary-colour secondary-colour]
  (let [stroke-width 10
        mask-id (gensym "m")
        triangles (->> (range 0 101 (int (/ 100 (* 2 n))))
                       rest
                       (map (fn [filled? dim]
                              [:svg {:width dim
                                     :height dim}
                               [:defs {}]
                               (polygon/equilateral
                                 3
                                 (- (float (/ dim
                                              2))
                                    stroke-width)
                                 {:style (format "fill:%s;"
                                                 (if filled?
                                                   "black"
                                                   "white")
                                                 stroke-width)})])
                            (cycle [true false]))
                       reverse
                       align/center)]
    [:svg {:width 100
           :height 100}
     [:defs {}
      [:mask {:id mask-id} triangles]]
     [:rect {:width 100
             :height 100
             :x 0 :y 0
             :fill (color/map-> (->graytone primary-colour))
             :mask (format "url(#%s)"
                           mask-id)}]]))

#_(defn needle-swatch
    [a b colour]
    (let [TODO--radius--percentage 0.1
          circle-radius (* TODO--radius--percentage
                           (line-length a b))]
      [:path {}]))

(defn needles--b-coord
  [a angle radius]
  :TODO)

(defn needle-coord
  [{:keys [width height]} padding]
  (let [TODO--radius 200
        coord-gen (fn [dimension]
                    (+ padding
                       (rand-int (- dimension (* 2
                                                 padding)))))
        a {:x (coord-gen width)
           :y (coord-gen height)}]
    {:a a
     :b (needles--b-coord a
                          (rand-int 360)
                          TODO--radius)}))

(defn needle-coords
  [dims padding n]
  (repeatedly n
              (partial needle-coord
                       dims
                       padding)))

(defn gen
  []
  (let [gen-fn identity]
    (doseq [[idx_1_based year month day] (e/indexed-days-of-week (e/week->date 2019 12))]
      (gen-fn idx_1_based year month day))))

(comment
  (trace/profile {} (gen)))
