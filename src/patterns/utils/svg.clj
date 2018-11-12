(ns patterns.utils.svg
  (:refer-clojure :exclude [use])
  (:require [clojure.string :as str]
            [patterns.utils :as utils]))

(defn dimensions
  "Return the topmost dimensions of `src` as {:height n :width n}"
  [src]
  (let [[_tag {:keys [width height] view-box :viewBox}] src]
    (cond (and width height)
          {:width width
           :height height}

          view-box
          (->> (str/split view-box #" ")
               (drop 2)
               (map #(Integer/valueOf ^String %))
               (zipmap [:width :height]))

          :else {})))

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

(defn to-svg-doc
  "Prep a Hiccup SVG to be a fully formatted SVG document."
  [[svg-tag attrs & contents]]
  {:pre [(= :svg svg-tag)]}
  (utils/veccat
    [:svg
     (assoc attrs :xmlns "http://www.w3.org/2000/svg"
                  :xmlns:xlink "http://www.w3.org/1999/xlink")]
    contents))
