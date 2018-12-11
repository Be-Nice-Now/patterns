(ns patterns.utils.svg
  (:refer-clojure :exclude [use])
  (:require [clojure.string :as str]
            [patterns.utils :as utils]
            [inkspot.color :as ink.color]))

(extend-type clojure.lang.PersistentArrayMap
  ink.color/IColor
  (red    [{:keys [r]}] r)
  (green  [{:keys [g]}] g)
  (blue   [{:keys [b]}] b)
  (alpha  [{:keys [a]}] (or a 1.0))
  (coerce [m] (ink.color/to-color m)))

(defn dimensions
  "Return the topmost dimensions of `src` as {:height n :width n}"
  [src]
  (let [[_tag {:keys [width height] view-box :viewBox}] src]
    (->> (cond (and width height)
               [width height]

               view-box
               (->> (str/split view-box #" ")
                    (drop 2))

               :else [])
         (map (fn [v]
                (or (and (string? v)
                         (Integer/valueOf ^String v))
                    v)))
         (zipmap [:width :height]))))

(defn- namespace-style
  [id style-str]
  (format "#%s %s"
          id style-str))

(defn- is-style?
  [el]
  (and (sequential? el)
       (= :style (first el))))

(defn ->def
  [src id]
  (let [[tag attrs defs & contents] src
        {:keys [width height]} (dimensions src)
        style-ns (str (gensym "id"))]
    [tag
     (-> attrs
         (assoc :id id
                :width width
                :height height))
     (conj (vec (remove is-style? defs))
           (utils/veccat
             [:style {}]
             (map (partial namespace-style style-ns)
                  (->> defs
                       (filter is-style?)
                       first
                       (drop 2)))))
     (utils/veccat
       [:g
        {:id style-ns}]
       contents)]))

(defn use
  "Define an SVG use element"
  [id {_x :x _y :y _w :width _h :height :as attrs}]
  [:use
   (assoc attrs :xlink:href (str "#" id))])

(defn line
  [_dimensions {[x1 y1] :start
                [x2 y2] :end}]
  [:path
   {:d (format "M %s %s L %s %s"
               x1
               y1
               x2
               y2)}])

(defn quadratic
  [{:keys [width height]}
   {[x1 y1] :start
    [x2 y2] :end}]
  [:path
   {:d (format "M %s %s Q %s %s, %s %s"
               x1 y1
               (/ width 2)
               (/ height 2)
               x2 y2)}])

(defn multi-stroke
  [strokes]
  (let [ids (repeatedly (count strokes) (partial gensym "d"))]
    {:style (str/join " "
                      (map (fn [id {width :width
                                    {:keys [r g b]} :colour}]
                             (format "#%s path {fill:none;stroke:rgb(%s,%s,%s);stroke-width:%s}"
                                     id (or r 0) (or g 0) (or b 0) width))
                           ids
                           strokes))
     :path-fn (fn [path]
                (utils/veccat [:g {}]
                              (map (fn [id]
                                     [:g {:id id} path])
                                   (reverse ids))))}))

(defn to-svg-doc
  "Prep a Hiccup SVG to be a fully formatted SVG document."
  [[svg-tag attrs & contents]]
  {:pre [(= :svg svg-tag)]}
  (utils/veccat
    [:svg
     (assoc attrs :xmlns "http://www.w3.org/2000/svg"
                  :xmlns:xlink "http://www.w3.org/1999/xlink")]
    contents))
