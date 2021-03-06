(ns patterns.tile
  (:require [patterns.utils :as utils]
            [patterns.utils.sequences :as utils.seq]
            [patterns.utils.svg :as svg]
            [patterns.utils.svg.polygon :as polygon]))

(defn transform-constant
  ":transform-fn for `grid` which acts like identity."
  [_ _ x]
  x)

(defn pick-alternate
  "Returns :pick-fn for `grid` which alternates through the srcs."
  [{srcs :srcs xn :xn} [x y]]
  (mod (+ x
          (* y xn))
       (count srcs)))

(defn pick-random
  "Returns :pick-fn for `grid` which chooses a random index for the srcs."
  [{srcs :srcs} _]
  (rand-int (count srcs)))

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
                               (float rotate-by)
                               (float (+ (/ width 2) x))
                               (float (+ (/ height 2) y)))))]
       content))))

(defn pi-rotate
  "Simple `transform-fn` where every unique tile is assigned an n quarter rotation,
   where n is the given digit of pi at the index of the tile.

   ie, rotate the given tile by:
   - 3 quarter turns
   - 1 quarter turn
   - 4 quarter turns
   - ..."
  [& args]
  (apply transform-rotate
         (fn [_src [x y] & _args]
           (mod (* 90
                   (get utils.seq/pi-seq
                        (mod (+ x y)
                             (count utils.seq/pi-seq))))
                360))
         args))

(defn grid
  "Given a sequence of `srcs` Hiccup svgs, return a Hiccup svg where they are
   tiled `xn` by `yn` times.

   Optionally takes:

   :transform-fn (fn [src [x y] element])
     A function which will be applied to every tiled element in the grid.

     Defaults to `transform-constant`

   :pick-fn (fn [{:srcs [src0 src1 ...] :xn xn :yn yn} [x y]])
     Given a sequence of the input `src`s, and the [x y] coordinate of the location
     in the grid, returns a number indicating which element to choose.

     Defaults to `pick-alternate`"
  [srcs xn yn & [{:keys [transform-fn
                         pick-fn]
                  :or {transform-fn transform-constant
                       pick-fn pick-alternate}}]]
  (let [picks (for [x (range 0 xn)
                    y (range 0 yn)]
                [[x y] (pick-fn {:srcs srcs
                                 :xn xn
                                 :yn yn}
                                [x y])])
        tile-prefix (gensym "tile")
        defs (->> picks
                  (map second)
                  (into #{})
                  (map (fn [i]
                         (svg/->def (nth srcs i) (str tile-prefix i)))))
        dimensions (map svg/dimensions defs)
        width (apply min (map :width dimensions))
        height (apply min (map :height dimensions))]
    (utils/veccat [:svg
                   {:viewBox (format "0 0 %s %s"
                                     (* xn width)
                                     (* yn height))}
                   (utils/veccat
                     [:defs {}]
                     defs)]
                  (map (fn [[[x y] src-idx]]
                         (transform-fn
                           (nth srcs src-idx)
                           [x y]
                           (svg/use (str tile-prefix src-idx)
                                    {:x (* x width)
                                     :y (* y height)
                                     :width width
                                     :height height})))
                       picks))))

#_"" "
- layout takes srcs and a [series of points/fn which generates points from x,y]
- pub:
  - grid/rectangle masks things by a rectangle
  - triangle masks things by a triangle
- (take n (cycle srcs))
" ""

(defn triangle
  [srcs xn yn & [{:keys [transform-fn
                         pick-fn]
                  :or {transform-fn transform-constant
                       pick-fn pick-alternate}}]]
  (let [clip-id-0 (gensym "tile--triangle--clip")
        clip-id-1 (gensym "tile--triangle--clip")
        clip-ids [clip-id-0 clip-id-1]
        picks (for [x (range 0 xn)
                    y (range 0 yn)]
                {:x x
                 :y y
                 :src-idx (pick-fn
                            {:srcs srcs
                             :xn xn
                             :yn yn}
                            [x y])
                 :clip-id (nth clip-ids
                               (pick-alternate
                                 {:srcs clip-ids
                                  :xn xn
                                  :yn yn}
                                 [x y]))})
        tile-prefix (gensym "tile")
        defs (->> picks
                  (map :src-idx)
                  (into #{})
                  (map (fn [i]
                         (let [[tag attrs & body] (nth srcs i)
                               id (str tile-prefix i)]
                           (svg/->def (utils/veccat
                                        [tag (assoc attrs
                                               :clip-path (format "url(#%s)"
                                                                  id))]
                                        body)
                                      (str tile-prefix i))))))
        dimensions (map svg/dimensions defs)
        width (Math/round (float (/ (apply min (map :width dimensions))
                                    2)))
        height (apply min (map :height dimensions))]
    (utils/veccat [:svg
                   {:width (* (inc xn) width)
                    :height (* yn height)}
                   (utils/veccat
                     [:defs {}
                      [:clipPath {:id clip-id-0}
                       (polygon/points->
                         [[0 height]
                          [width 0]
                          [(* 2 width) height]])]
                      [:clipPath {:id clip-id-1}
                       (polygon/points->
                         [[0 0]
                          [width height]
                          [(* 2 width) 0]])]]
                     defs)]
                  (map (fn [{:keys [x y src-idx clip-id]}]
                         (transform-fn
                           (nth srcs src-idx)
                           [x y]
                           (svg/use (str tile-prefix src-idx)
                                    {:x (* x width)
                                     :y (* y height)
                                     :clip-path (format "url(#%s)"
                                                        clip-id)})))
                       picks))))
