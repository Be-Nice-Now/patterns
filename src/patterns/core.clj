(ns patterns.core
  (:require [clojure.string :as str]
            [dali.io :as io]
            [hiccup.core :as to-html]
            [clojure.data.xml :as xml]))

(defn render
  "Passthrough to dali.io/render-svg"
  [src filename]
  (io/render-svg src
                 filename))

(def to-tile (first
               (io/enlive->hiccup
                 (io/load-enlive-svg
                   "./pipes-01.svg"))))

(def to-tile-0
  [:svg
   {:xmlns "http://www.w3.org/2000/svg", :xmlns:xlink "http://www.w3.org/1999/xlink", :viewBox "0 0 700 700"}
   [:defs
    [:style
     "#tile-0_0{clip-path:url(#clip-path--tile-0_0);}.cls-3{fill:#09ff21;stroke:#f0f;stroke-miterlimit:10;stroke-width:50px;}"]
    [:clipPath {:id "clip-path--tile-0_0"} [:rect {:class "cls-1", :width "700", :height "700"}]]]
   [:title "pipes"]
   [:g
    {:id "tile-0_0", :class "cls-2"}
    [:rect {:class "cls-3", :x "-56.02", :y "199.92", :width "869.21", :height "300.08"}]
    [:rect
     {:class "cls-3",
      :x "-84.6",
      :y "199.92",
      :width "869.21",
      :height "300.08",
      :transform "translate(699.96 -0.04) rotate(90)"}]
    [:path
     {:class "cls-3",
      :d "M700.12,500a497.08,497.08,0,0,1-194.71-39.36A498.27,498.27,0,0,1,346.48,353.52,498.27,498.27,0,0,1,239.36,194.59,497.08,497.08,0,0,1,200-.12V-75H499.87L500-.12C500,110.23,589.77,200,700.12,200l113.07.22V500Z"}]]])

(def document
  [:dali/page
   [:circle
    {:stroke :indigo :stroke-width 4 :fill :darkorange}
    [30 30] 20]])

(defn dimensions
  "Given a Hiccup svg, return the topmost dimensions as {:height n :width n}"
  [[svg-tag {view-box :viewBox}]]
  {:pre [(= :svg svg-tag)]}
  (->> (str/split view-box #" ")
       (drop 2)
       (map #(Integer/valueOf ^String %))
       (zipmap [:width :height])))

(defn assoc-id
  "Given a Hiccup svg `src`, add :id `id`"
  [[svg-tag {attr-id :id :as attrs} & contents] id]
  {:pre [(= :svg svg-tag)
         (nil? attr-id)]}
  (concat [:svg
           (assoc attrs :id id)]
          contents))

(defn id-href-image
  [id x y]
  [:image
   {:xlink:href (str "#" id)
    :width "100%"
    :height "100%"
    :x x
    :y y}])

(defn grid
  [src xn yn]
  (let [{:keys [width height]} (dimensions src)]
    (concat [:svg
             {:viewBox (format "0 0 %s %s"
                               (* xn width)
                               (* yn height))}
             [:defs
              (assoc-id src "base-tile")]]
            (for [x (range 0 xn)
                  y (range 0 yn)]
              (id-href-image "base-tile"
                             (* x width)
                             (* y height))))))

(comment
  (from-html/parse "./pipes-01.svg")
  (dimensions to-tile)
  (assoc-id to-tile "base-tile")
  (-> {:document [:dali/page
                  (grid to-tile-0 3 3)]}
      dali.syntax/dali->ixml
      dali.layout/resolve-layout
      ;dali.syntax/ixml->xml
      ;dali.io/xml->svg-document-string
      )
  (dali.io/render-svg
    {:document (grid to-tile-0 3 3)}
    "./pipes-02.svg"))
