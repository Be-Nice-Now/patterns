(ns patterns.tile
  (:require [patterns.utils :as utils]
            [patterns.utils.svg :as svg]
            [patterns.hiccup :as hiccup]))

(defn grid
  "Given a src Hiccup svg, return a Hiccup svg where it is
   tiled xn by yn times.

   Optionally takes a :transform-fn, which is a function which will
   be applied to every tiled element in the grid:

   (fn transform-fn [src [x y] element])"
  ([src xn yn]
   (grid src xn yn {:transform-fn (fn [_ _ x] x)}))
  ([src xn yn {:keys [transform-fn]}]
   (let [{:keys [width height]} (svg/dimensions src)]
     (utils/veccat [:svg
                    {:viewBox (format "0 0 %s %s"
                                      (* xn width)
                                      (* yn height))}
                    [:defs
                     (svg/->defs src "base-tile")]]
                   (for [x (range 0 xn)
                         y (range 0 yn)]
                     (transform-fn
                       src
                       [x y]
                       (svg/use "base-tile"
                                (* x width)
                                (* y height))))))))

(defn rotate
  "Returns a :transform-fn for `grid` which rotates src about
   the center of each tile.

   Optionally takes a :rotate-by-fn, which is a function which
   will be used to determine the rotation of each tile:

   (fn rotate-by [src [x y] element]) => n in [0 ... 359]

   Defaults to (* 90 (rand-int 4))"
  ([src row-column el]
   (rotate (fn [& _args]
             (* 90 (rand-int 4)))
           src row-column el))
  ([rotate-by-fn src row-column el]
   (let [[tag {:keys [x y] :as attrs} & content] el
         {:keys [width height]} (svg/dimensions src)
         rotate-by (rotate-by-fn src row-column el)]
     (utils/veccat
       [tag
        (assoc attrs
          :transform (when-not (zero? rotate-by)
                       (format "rotate(%s, %s, %s)"
                               rotate-by
                               (+ (/ width 2) x)
                               (+ (/ height 2) y))))]
       content))))
