(ns patterns.shatter
  (:require [patterns.utils.svg :as svg]
            [patterns.core :as patterns]
            [clojure.string :as str]
            [patterns.utils :as utils]))

(defn fe-matrix
  [id in values]
  [:feColorMatrix {:result id
                   :in in
                   :type "matrix"
                   :values (->> values
                                (map (partial str/join " "))
                                (str/join " "))}])

(defn shatter
  [src padding mode bitmap-matrix]
  (let [src-id (str (gensym "s"))
        filter-id (str "f" src-id)
        dims (svg/dimensions src)]
    [:svg dims
     (utils/veccat
       [:defs {}
        (svg/->def src src-id)
        (utils/veccat
          [:filter {:id filter-id
                    :x 0 :y 0
                    :width "100%" :height "100%"}]
          (for [i (range 3)]
            [:feImage {:result (str "i" filter-id i)
                       :xlink:href (str "#" src-id i)}])
          (for [i (range 3)]
            (fe-matrix (str "m" filter-id i)
                       (str "i" filter-id i)
                       (-> bitmap-matrix
                           (assoc-in [i i] 1)
                           (assoc-in [i 4] 0))))
          [[:feBlend {:result (str "b" filter-id)
                      :in (str "m" filter-id 0)
                      :in2 (str "m" filter-id 1)
                      :mode mode}]
           [:feBlend {:in (str "m" filter-id 2)
                      :in2 (str "b" filter-id)
                      :mode mode}]])]
       [(svg/use src-id {:id (str src-id 0)
                         :x 0
                         :y 0
                         :transform (format "translate(%s, %s)"
                                            0 (- padding))})
        (svg/use src-id {:id (str src-id 1)
                         :x 0
                         :y 0
                         :transform (format "translate(%s, %s)"
                                            (- padding) padding)})
        (svg/use src-id {:id (str src-id 2)
                         :x 0
                         :y 0
                         :transform (format "translate(%s, %s)"
                                            padding padding)})])
     [:rect (assoc dims
              :x 0 :y 0
              :style (format "filter:url(#%s);"
                             filter-id))]]))

(defn light
  [src padding]
  (shatter src padding "lighten" [[0 0 0 0 0]
                                  [0 0 0 0 0]
                                  [0 0 0 0 0]
                                  [0 0 0 1 0]]))

(defn dark
  [src padding]
  (shatter src padding "darken" [[0 0 0 0 255]
                                 [0 0 0 0 255]
                                 [0 0 0 0 255]
                                 [0 0 0 1 0]]))
