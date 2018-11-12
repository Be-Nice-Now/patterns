(ns patterns.tile
  (:require [patterns.utils :as utils]
            [patterns.utils.svg :as svg]))

(defn transform-constant
  ":transform-fn for `grid` which acts like identity."
  [_ _ x]
  x)

(defn pick-alternate
  "Returns :pick-fn for `grid` which alternates through the srcs."
  [srcs [x y]]
  (mod (+ x y)
       (count srcs)))

(defn transform-rotate
  "A :transform-fn for `grid` which rotates src about
   the center of each tile.

   Optionally takes a :rotate-by-fn, which is a function which
   will be used to determine the rotation of each tile:

   (fn rotate-by [src [x y] element]) => n in [0 ... 359]

   Defaults to (* 90 (rand-int 4))

   Suggested usage is (partial rotate your-rotate-by-fn)"
  ([src row-column el]
   (transform-rotate (fn [& _args]
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

(defn grid
  "Given a sequence of `srcs` Hiccup svgs, return a Hiccup svg where they are
   tiled `xn` by `yn` times.

   Optionally takes:

   :transform-fn (fn [src [x y] element])
     A function which will be applied to every tiled element in the grid.

     Defaults to `transform-constant`

   :pick-fn (fn [[src0 src1 ...] [x y]])
     Given a sequence of the input `src`s, and the [x y] coordinate of the location
     in the grid, returns a number indicating which element to choose.

     Defaults to `pick-alternate`"
  [srcs xn yn & [{:keys [transform-fn
                         pick-fn]
                  :or {transform-fn transform-constant
                       pick-fn pick-alternate}}]]
  (let [dimensions (map svg/dimensions srcs)
        width (apply min (map :width dimensions))
        height (apply min (map :height dimensions))]
    (utils/veccat [:svg
                   {:viewBox (format "0 0 %s %s"
                                     (* xn width)
                                     (* yn height))}
                   (utils/veccat
                     [:defs]
                     (for [i (range (count srcs))]
                       (svg/->def (get srcs i) (str "base-tile--" i))))]
                  (for [x (range 0 xn)
                        y (range 0 yn)]
                    (let [picked-n (pick-fn srcs [x y])]
                      (transform-fn
                        (get srcs picked-n)
                        [x y]
                        (svg/use (str "base-tile--" picked-n)
                                 {:x (* x width)
                                  :y (* y height)
                                  :width width
                                  :height height})))))))
