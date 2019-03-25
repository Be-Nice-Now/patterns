(ns patterns.examples.2019-11
  (:require [clojure.string :as str]
            [patterns.examples :as e]
            [patterns.shatter :as shatter]
            [patterns.utils :as utils]
            [taoensso.tufte :as trace]))

(defn- ->int
  [n]
  (-> n
      float
      Math/round
      int))

(defn- circle-intersects
  [{:keys [x y radius]} horizontal-line--y]
  (let [y0 (- horizontal-line--y
              y)
        sqrt_+- (-> (Math/sqrt
                      (float (- (* radius radius)
                                (* y0 y0))))
                    ->int)]
    [(+ x
        sqrt_+-)
     (- x
        sqrt_+-)]))

(defn- ->exterior-line
  [endpoints {large-radius :radius
              :as circle} small-radius]
  (let [[{leftmost--y :y
          :as left-endpoint}
         {rightmost--y :y
          :as right-endpoint}] (sort-by :x endpoints)
        is-facing-up? (< leftmost--y (:y circle))
        left-circle-intersect--y (if is-facing-up?
                                   (- leftmost--y small-radius)
                                   (+ leftmost--y small-radius))
        right-circle-intersect--y (if is-facing-up?
                                    (- leftmost--y small-radius)
                                    (+ leftmost--y small-radius))
        left-circle-intersect--x (-> (circle-intersects circle
                                                        left-circle-intersect--y)
                                     sort
                                     first)
        right-circle-intersect--x (-> (circle-intersects circle
                                                         right-circle-intersect--y)
                                      sort
                                      last)
        ;; calculated points
        left-line-intersect-point {:x (- left-circle-intersect--x
                                         small-radius)
                                   :y leftmost--y}
        left-circle-intersect {:x left-circle-intersect--x
                               :y left-circle-intersect--y}
        right-circle-intersect {:x right-circle-intersect--x
                                :y right-circle-intersect--y}
        right-line-intersect-point {:x (+ right-circle-intersect--x
                                          small-radius)
                                    :y rightmost--y}]
    [[:line {:endpoints [left-endpoint left-line-intersect-point]}]
     [:arc {:radii {:x small-radius :y small-radius}
            :x-rot 0
            :large? false
            :sweep? false
            :endpoints [left-line-intersect-point
                        left-circle-intersect]}]
     [:arc {:radii {:x large-radius :y large-radius}
            :x-rot 0
            :large? false
            :sweep? true
            :endpoints [left-circle-intersect
                        right-circle-intersect]}]
     [:arc {:radii {:x small-radius :y small-radius}
            :x-rot 0
            :large? false
            :sweep? false
            :endpoints [right-circle-intersect
                        right-line-intersect-point]}]
     [:line {:endpoints [right-line-intersect-point right-endpoint]}]]))

(defn- ->interior-line
  [endpoints circle center-y
   [bezier-x0
    bezier-x1
    bezier-x2
    bezier-x3]]
  (let [[{leftmost--y :y
          :as left-endpoint}
         {rightmost--y :y
          :as right-endpoint}] (sort-by :x endpoints)
        left-circle-intersect--x (-> (circle-intersects circle
                                                        leftmost--y)
                                     sort
                                     first)
        right-circle-intersect--x (-> (circle-intersects circle
                                                         rightmost--y)
                                      sort
                                      last)
        ;; calculated points
        left-circle-intersect {:x left-circle-intersect--x
                               :y leftmost--y}
        center {:x (->int (+ (/ (- bezier-x2 bezier-x1)
                                2)
                             bezier-x1))
                :y center-y}
        right-circle-intersect {:x right-circle-intersect--x
                                :y rightmost--y}]
    [[:line {:endpoints [left-endpoint left-circle-intersect]}]
     [:bezier {:endpoints [(assoc left-circle-intersect
                             :b-x bezier-x0)
                           (assoc center
                             :b-x bezier-x1)]}]
     [:bezier {:endpoints [(assoc center
                             :b-x bezier-x2)
                           (assoc right-circle-intersect
                             :b-x bezier-x3)]}]
     [:line {:endpoints [right-circle-intersect right-endpoint]}]]))

(defn- reverse-path
  [path]
  (->> path
       (map (fn [[tag attrs]]
              [tag (update attrs :endpoints (comp vec reverse))]))
       reverse
       vec))

(defn- join-paths
  "Given two paths, joins the lines by adding joining lines for each."
  [a b]
  (let [first-point (fn [path]
                      (-> path
                          first
                          second
                          :endpoints
                          first))
        last-point (fn [path]
                     (-> path
                         last
                         second
                         :endpoints
                         last))]
    (utils/veccat
      a
      [[:line {:endpoints [(last-point a) (first-point b)]}]]
      b
      [[:line {:endpoints [(last-point b) (first-point a)]}]])))

(defn- line->d
  [{[_ {:keys [x y]}] :endpoints}]
  (format "L %s %s"
          x y))

(defn- arc->d
  [{{arc-x :x arc-y :y} :radii
    [_ {:keys [x y]}] :endpoints
    :keys [x-rot large? sweep?]}]
  (format "A %s %s, %s, %s, %s, %s %s"
          arc-x arc-y
          x-rot
          (if large?
            1
            0)
          (if sweep?
            1
            0)
          x y))

(defn- bezier-w-defaults
  [{:keys [x y b-x b-y]}]
  {:x x
   :y y
   :b-x (or b-x x)
   :b-y (or b-y y)})

(defn- bezier->d
  [{:keys [endpoints]}]
  (let [[{:keys [b-x b-y]}
         {b1-x :b-x
          b1-y :b-y
          :keys [x y]}]
        (map bezier-w-defaults endpoints)]
    (format "C %s %s, %s %s, %s %s"
            b-x b-y
            b1-x b1-y
            x y)))

(defn- tag->d
  [[tag attrs]]
  (case tag
    :line (line->d attrs)
    :arc (arc->d attrs)
    :bezier (bezier->d attrs)))

(defn- path->d
  [path]
  (let [[[_tag {[{:keys [x y]}] :endpoints}]] path]
    (format "M %s %s %s Z"
            x y
            (str/join
              " "
              (map tag->d path)))))

(defn gen
  []
  (let [small-radius 10
        shatter-padding 5
        n-rands (fn [n]
                  (sort (repeatedly n rand)))
        circle-radius (-> e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                          (* (/ 7 8))
                          (* (/ 1 2))
                          ->int)
        gap (->int (/ (- e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                         (* circle-radius 2))
                      2))
        center (->int (/ e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                         2))
        circle {:x center
                :y center
                :radius circle-radius}
        gen-fn (fn [idx_1_based year month day]
                 (let [n-interior-lines (* 2 idx_1_based)
                       ys (->> n-interior-lines
                               n-rands
                               (map (partial * circle-radius 2))
                               (map (partial + gap))
                               (map ->int))
                       top-half-ys (take-while (partial > center) ys)
                       bottom-half-ys (drop (count top-half-ys) ys)
                       endpoint-ys (->> n-interior-lines
                                        range
                                        (map inc)
                                        (map (fn [i]
                                               (* (/ (* 2 gap)
                                                     (inc n-interior-lines))
                                                  i)))
                                        (map ->int)
                                        (map (partial + (- center gap))))
                       ->bezier-x-seeds (fn [ys]
                                          (->> (count ys)
                                               (* 4)
                                               n-rands))
                       ->bezier-x (fn [y seed]
                                    (let [[beginning end] (sort
                                                            (circle-intersects
                                                              circle
                                                              y))]
                                      (->int (+ beginning
                                                (* (- end beginning)
                                                   seed)))))
                       top-half-bezier-seeds (->bezier-x-seeds top-half-ys)
                       bottom-half-bezier-seeds (->bezier-x-seeds bottom-half-ys)
                       ->bezier-xs (fn [endpoint-y center-y [b0 b1 b2 b3]]
                                     (let [[y0 y1] (sort [endpoint-y center-y])
                                           middle-y (+ y0
                                                       (/ (- y1 y0)
                                                          2))]
                                       [(->bezier-x middle-y b0)
                                        (->bezier-x middle-y b1)
                                        (->bezier-x middle-y b2)
                                        (->bezier-x middle-y b3)]))
                       ->interior-line (fn [endpoint-y center-y [b0 b1] [b3 b2]]
                                         (->interior-line
                                           [{:x 0
                                             :y endpoint-y}
                                            {:x e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                                             :y endpoint-y}]
                                           circle center-y
                                           (->bezier-xs endpoint-y
                                                        center-y
                                                        [b0 b1 b2 b3])))
                       ->interior-lines (fn [endpoint-ys center-ys bezier-seeds]
                                          (map
                                            ->interior-line
                                            endpoint-ys
                                            center-ys
                                            (partition 2 bezier-seeds)
                                            (partition 2 (reverse bezier-seeds))))
                       paths (utils/veccat
                               [(->exterior-line [{:x 0
                                                   :y (- center gap)}
                                                  {:x e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                                                   :y (- center gap)}]
                                                 circle small-radius)]
                               (->interior-lines
                                 endpoint-ys
                                 top-half-ys
                                 top-half-bezier-seeds)
                               (reverse
                                 (->interior-lines
                                   (reverse (take-last (count bottom-half-ys) endpoint-ys))
                                   (reverse bottom-half-ys)
                                   bottom-half-bezier-seeds))
                               [(->exterior-line [{:x 0
                                                   :y (+ center gap)}
                                                  {:x e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                                                   :y (+ center gap)}]
                                                 circle small-radius)])
                       compound-paths (map (fn [[a b]]
                                             (join-paths a (reverse-path b)))
                                           (partition 2 paths))]

                   (e/render [year month day]
                             (shatter/dark
                               (utils/veccat
                                 [:svg {:width e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                                        :height e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT}
                                  [:defs {}]
                                  [:rect {:fill "whitesmoke"
                                          :x 0 :y 0
                                          :width e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                                          :height e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT}]]
                                 (->> compound-paths
                                      (map path->d)
                                      (map (fn [d]
                                             [:path {:fill "midnightblue"
                                                     :d d}]))))
                               shatter-padding)
                             (format ""))))]
    (doseq [[idx_1_based year month day] (e/indexed-days-of-week (e/week->date 2019 11))]
      (gen-fn idx_1_based year month day))))

(comment
  (trace/profile {} (gen)))
