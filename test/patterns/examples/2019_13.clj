(ns patterns.examples.2019-13
  (:require [patterns.examples :as e]
            [taoensso.tufte :as trace]))


(defn gen
  []
  (let [gen-fn (fn [idx_1_based year month day [primary-colour secondary-colour]]
                 (e/render [year month day]
                           [:svg {:width e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT
                                  :height e/INSTAGRAM-RECOMMENDED-MIN-WIDTH-HEIGHT}
                            [:defs {}]
                            [:rect {}]]
                           (e/format-w-newlines
                             [["%s"]
                              ["%s"]
                              ["%s"]]
                             year month day)
                           {:recursive? false}))
        colours (->> e/poke-palettes
                     (shuffle)
                     (take 7)
                     (map (comp (partial take 4) rest))
                     (map (fn [xs]
                            (map second
                                 xs))))]
    (doseq [[[idx_1_based year month day] colours]
            (map list
                 (e/indexed-days-of-week (e/week->date 2019 13))
                 colours)]
      (gen-fn idx_1_based year month day colours))))

(comment
  (trace/profile {} (gen)))
