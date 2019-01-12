(ns patterns.utils.svg.filter
  (:require [clojure.string :as str]))

(defn matrix
  [id in values]
  [:feColorMatrix {:result id
                   :in in
                   :type "matrix"
                   :values (->> values
                                (map (partial str/join " "))
                                (str/join " "))}])
