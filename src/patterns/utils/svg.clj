(ns patterns.utils.svg
  (:require [clojure.string :as str]
            [patterns.utils :as utils]))

(defn dimensions
  "Given a Hiccup SVG, return the topmost dimensions as {:height n :width n}"
  [[svg-tag {:keys [width height] view-box :viewBox}]]
  {:pre [(= :svg svg-tag)]}
  (cond (and width height)
        {:width width
         :height height}

        view-box
        (->> (str/split view-box #" ")
             (drop 2)
             (map #(Integer/valueOf ^String %))
             (zipmap [:width :height]))

        :else {}))

(defn ->defs
  [src id]
  (let [[tag attrs & contents] src
        {:keys [width height]} (dimensions src)]
    (utils/veccat [tag
                   (-> attrs
                       (dissoc :viewBox)
                       (assoc :id id
                              :width width
                              :height height))]
                  contents)))

(defn use
  "Define an SVG use element"
  [id x y]
  [:use
   {:href (str "#" id)
    :x x
    :y y}])

(defn to-svg-doc
  "Prep a Hiccup SVG to be a fully formatted SVG document."
  [[svg-tag attrs & contents]]
  {:pre [(= :svg svg-tag)]}
  (utils/veccat
    [:svg
     (assoc attrs :xmlns "http://www.w3.org/2000/svg")]
    contents))
