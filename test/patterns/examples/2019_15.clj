(ns patterns.examples.2019-15
  (:require [patterns.examples :as e]
            [taoensso.tufte :as trace]
            [patterns.tile :as tile]
            [patterns.utils.layout.align :as align]
            [patterns.utils.svg.color :as color]
            [patterns.utils :as utils]
            [patterns.examples.2019-13 :refer [->int vector-field]]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

(defn- colour-stream-steps
  [channel start stop n]
  (let [a (channel start)
        b (channel stop)]
    (->> (cond (= a b) (repeat (inc n) a)
               (< a b) (range a (inc b)
                              (/ (- b a)
                                 n))
               :else (range a (dec b)
                            (/ (- b a)
                               n)))
         (map ->int))))

(defn- colour-streams-steps
  [start stop n]
  (map (fn [a r g b]
         {:a a
          :r r
          :g g
          :b b})
       (colour-stream-steps :a start stop n)
       (colour-stream-steps :r start stop n)
       (colour-stream-steps :g start stop n)
       (colour-stream-steps :b start stop n)))

(defn colour-steps
  [start stop n]
  (let [steps (colour-streams-steps start stop n)]
    (map list
         steps
         (rest steps))))

(defn- quote->dimension
  [quote]
  (apply max
         (count quote)
         (map count
              quote)))

(defn gen
  []
  (let [gen-fn (fn [idx_1_based year month day quote
                    [background--start
                     background--stop]
                    [text--start
                     text--stop]]
                 (let [tiles-xy (quote->dimension quote)
                       tile-dimension (->int (/ e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                                                tiles-xy))
                       tile-dims {:width tile-dimension
                                  :height tile-dimension}
                       blank-tile [:svg tile-dims
                                   [:defs {}]]
                       character-dims {:width (->int (/ tile-dimension 2))
                                       :height (->int (/ tile-dimension 2))}
                       srcs (->> quote
                                 (mapcat (fn [line]
                                           (utils/veccat
                                             (map (fn [[_ colour] c]
                                                    (if (str/blank? (str c))
                                                      blank-tile
                                                      (e/character-tile-gen
                                                        tile-dims
                                                        character-dims
                                                        (constantly colour)
                                                        (str c))))
                                                  (colour-steps text--start text--stop (count line))
                                                  line)
                                             (repeat (- tiles-xy (count line))
                                                     blank-tile)))))
                       srcs-padded-with-blank-lines (utils/veccat
                                                      srcs
                                                      (repeat (* (- tiles-xy
                                                                    (count quote))
                                                                 tiles-xy)
                                                              blank-tile))]
                   (e/render [year month day]
                             (align/center
                               [[:svg {:width e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                                       :height e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT}
                                 [:defs {}
                                  [:linearGradient {:id "gradient"}
                                   [:stop {:style (str "stop-color:" (color/map-> background--start))
                                           :offset "0%"}]
                                   [:stop {:style (str "stop-color:" (color/map-> background--stop))
                                           :offset "100%"}]]]
                                 [:rect {:x 0 :y 0
                                         :width e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                                         :height e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                                         :fill "url(#gradient)"}]]
                                (tile/grid
                                  srcs-padded-with-blank-lines
                                  tiles-xy tiles-xy)]
                               {:clip? {:width e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                                        :height e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT}})
                             (e/format-w-newlines
                               [["This week we're taking adages and expressions"
                                 "from Jerry Brasier. You just might recognize"
                                 "our favourite quote from him at the end of the week"
                                 ";)"]
                                [(str/join " " quote)]
                                ["Characters for the text are randomly pulled"
                                 "from the EMNIST dataset."]
                                ["https://en.wikipedia.org/wiki/MNIST_database"]]
                               idx_1_based))))
        quotes [["Finer"
                 "Than frog hairs"
                 "Split"
                 "Five ways"]
                ["Im not sleeping"
                 "Im checking my eyes"
                 "For light leaks"]
                ["Im a mushroom"
                 "They keep me in the dark"
                 "take me nowhere"
                 "and feed me scraps"]
                ["Im"
                 "wonderful"
                 "just"
                 "like"
                 "you"]
                ["Were"
                 "doin"
                 "it"]
                ["I just"
                 "worry so"
                 "about it"]
                ["be"
                 "NICE"
                 "now"]]
        [start stop & palette] (->> e/poke-palettes
                                    (filter (fn [palette]
                                              (< 2 (count palette))))
                                    (shuffle)
                                    first
                                    (map second))
        gradient-steps (colour-steps start stop (count quotes))
        text-steps (colour-steps stop (last palette) (count quotes))]
    (doseq [[[idx_1_based year month day]
             quote
             background-step
             text-step] (map list
                             (e/indexed-days-of-week (e/week->date 2019 15))
                             quotes
                             gradient-steps
                             text-steps)]
      (gen-fn idx_1_based year month day quote background-step text-step))))

(comment
  (trace/profile {} (gen)))
