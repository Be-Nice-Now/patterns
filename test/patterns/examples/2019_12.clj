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
  (let [max-rgb (max r g b)
        diff (if (> ideal-gray
                    max-rgb)
               (- ideal-gray
                  (max r g b))
               (int (/ (- 255
                          max-rgb)
                       2)))]
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
  (let [stroke-width 2]
    (->> (range 100 stroke-width (- (/ (- 100
                                          stroke-width)
                                       n)))
         (map #(-> %
                   float
                   Math/round
                   int))
         (map (fn [dim]
                [:svg {:width (* 2 (+ dim stroke-width))
                       :height (* 2 (+ dim stroke-width))}
                 [:defs {}]
                 (polygon/equilateral
                   3
                   (- dim
                      stroke-width)
                   {:style (format "fill:none;stroke:%s;stroke-width:%s"
                                   (color/map-> (->graytone secondary-colour))
                                   stroke-width)})]))
         align/center)))

(defn- triangle-transform
  [{src-width :width
    src-height :height} {:keys [width height]} id]
  (let [scale (float (inc (rand)))
        x (int (+ (rand-int (- width
                               (* 2 src-width scale)))
                  (* src-width scale)))
        y (int (+ (rand-int (- height
                               (* 2 src-height scale)))
                  (* src-height scale)))]
    (svg/use id {:x x
                 :y y
                 :transform (format "rotate(%s %s %s) scale(%s)"
                                    (rand-int 360)
                                    (int (+ x (/ src-width
                                                 2)))
                                    (int (+ y (/ src-height
                                                 2)))
                                    scale)})))

(defn gen
  []
  (let [gen-fn (fn [idx_1_based year month day [primary-colour secondary-colour]]
                 (let [triangle-id (gensym "t")
                       triangle-swatch (triangle-swatch day
                                                        primary-colour
                                                        secondary-colour)]
                   (e/render [year month day]
                             (utils/veccat
                               [:svg {:width e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                                      :height e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT}
                                [:defs {}
                                 (svg/->def triangle-swatch
                                            triangle-id)]
                                (background-swatch (->graytone primary-colour))]
                               (repeatedly (* month day)
                                           #(triangle-transform
                                              (svg/dimensions triangle-swatch)
                                              {:width e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                                               :height e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT}
                                              triangle-id)))
                             ""
                             {:recursive? false})))
        colours (->> e/poke-palettes
                     (shuffle)
                     (take 7)
                     (map (comp (partial take 4) rest))
                     (map (fn [xs]
                            (map second
                                 xs))))]
    (doseq [[[idx_1_based year month day] colours]
            (map list
                 (e/indexed-days-of-week (e/week->date 2019 12))
                 colours)]
      (gen-fn idx_1_based year month day colours))))

(comment
  (trace/profile {} (gen)))
