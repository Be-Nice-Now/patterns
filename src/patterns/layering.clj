(ns patterns.layering
  (:require [patterns.utils.svg :as svg]
            [patterns.utils :as utils]
            [patterns.utils.svg.filter :as svg.filter]
            [patterns.shatter :as shatter]
            [clojure.string :as str]
            [patterns.core :as patterns]))

(defn applesauce
  [src angle n angle->radius-fn]
  (let [id (gensym "s")
        {:keys [height width]} (svg/dimensions src)
        diagonal (int (Math/ceil (Math/sqrt (double (+ (* width width)
                                                       (* height height))))))
        angles-radii (map (fn [a]
                            {:angle a
                             :radius (angle->radius-fn a)})
                          (take n (range 0 (* n angle) angle)))
        max-radius (apply max (map :radius angles-radii))
        center (* 2 (+ diagonal max-radius))]
    [:svg {:width (* 2 center)
           :height (* 2 center)}
     [:defs {}
      (svg/->def src id)]
     (utils/veccat
       [:g {:transform (format "translate(%s,%s)"
                               center
                               center)}]
       (for [{:keys [angle radius]} angles-radii]
         (if (zero? radius)
           (svg/use id {:transform (format "rotate(%s)"
                                           angle)})
           [:g {:transform (format "rotate(%s)"
                                   angle)}
            (svg/use id {:transform (format "translate(%s)"
                                            (angle->radius-fn angle))})])))]))

(defn rotate
  [src angle n]
  (applesauce src angle n (constantly 0)))

(defn infinite
  [src n]
  (let [id (gensym "s")
        mask-id (gensym "m")
        filter-id (gensym "f")
        dims (svg/dimensions src)
        round-to 10000
        angle (/ (Math/floor (* round-to (/ 360 n)))
                 round-to)
        angles (range 0 360 angle)]
    (rotate
      [:svg dims
       [:defs {}
        (svg/->def src id)
        [:filter {:id filter-id
                  :x 0 :y 0
                  :width "100%" :height "100%"}
         [:feImage {:result (str "i" filter-id)
                    :xlink:href (str "#" id)}]
         (svg.filter/matrix (gensym "f")
                            (str "i" filter-id)
                            [[0 0 0 0 0]
                             [0 0 0 0 0]
                             [0 0 0 0 0]
                             [0 0 0 1 0]])]
        (utils/veccat
          [:mask {:id mask-id}
           [:rect (assoc dims
                    :fill "white")]]
          (for [angle (take-while (partial > 90) (rest angles))]
            [:rect (assoc dims
                     :style (format "filter:url(#%s);"
                                    filter-id)
                     :transform (format "rotate(%s)"
                                        angle))]))]
       (svg/use id {:mask (format "url(#%s)"
                                  mask-id)})]
      angle
      n)))

(comment
  (def src (shatter/dark
             [:svg {:width 500 :height 500}
              [:defs {}]
              [:polygon {:points (->> (range 0 (* 2 Math/PI) (/ (* 2 Math/PI) 3))
                                      (take 3)
                                      (map (fn [degree]
                                             (format "%s,%s"
                                                     (+ 250 (int (* 250 (Math/cos (double degree)))))
                                                     (+ 250 (int (* 250 (Math/sin (double degree))))))))
                                      (str/join " "))}]]
             10))
  (def cube
    [:svg {:width 300 :height 300}
     [:defs {}]
     [:polygon {:points "150,0 300,75 150,150 0,75"
                :style "stroke-width:10;stroke:rgb(0,255,255)"}]
     [:polygon {:points "150,300 150,150 300,75 300,225"
                :style "stroke-width:10;stroke:rgb(255,0,255)"}]
     [:polygon {:points "150,300 150,150 0,75 0,225"
                :style "stroke-width:10;stroke:rgb(255,255,0)"}]])
  (def stairs
    (utils/veccat
      [:svg {:width 550 :height 575}
       [:defs {}
        [:g {:id "step"}
         [:polygon {:points "150,200 150,150 300,75 300,125"
                    :style "stroke-width:10;stroke:black"}]
         [:polygon {:points "150,0 300,75 150,150 0,75"
                    :style "stroke-width:10;stroke:rgb(0,255,255)"}]
         [:polygon {:points "150,200 150,150 0,75 0,125"
                    :style "stroke-width:10;stroke:rgb(255,0,255)"}]]]]
      (for [[x y] (->> (map list (range 0 1000 50) (range 0 1000 75))
                       (take 6)
                       reverse)]
        (svg/use "step" {:transform (format "translate(%s,%s)"
                                            (- 250 x) y)}))))
  (patterns/render
    "tmp-stairs"
    stairs
    :png)
  (patterns/render
    "tmp-spiral"
    (applesauce src
                10
                500
                (fn [x]
                  (/ x 5)))
    :png)
  (patterns/render
    "tmp-stairs-infinite"
    (infinite stairs
              5)
    :png)
  (patterns/render
    "tmp-cube-infinite"
    (infinite cube
              5)
    :png)
  (patterns/render
    "tmp-infinite"
    (infinite src
              100)
    :png)
  (patterns/render
    "tmp-c"
    (rotate src
            3
            100)
    :png))

