(ns patterns.utils.sequences)

(def pi-seq (vec (map #(Integer/valueOf (str %))
                      (clojure.string/replace
                        (str
                          (Math/PI))
                        #"\." ""))))
