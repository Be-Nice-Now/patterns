(ns patterns.examples.2019-12
  (:require [patterns.examples :as e]
            [taoensso.tufte :as trace]))

(defn gen
  []
  (let [gen-fn identity]
    (doseq [[idx_1_based year month day] (e/indexed-days-of-week (e/week->date 2019 12))]
      (gen-fn idx_1_based year month day))))

(comment
  (trace/profile {} (gen)))
