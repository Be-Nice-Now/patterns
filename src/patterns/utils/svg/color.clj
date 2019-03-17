(ns patterns.utils.svg.color)

(defn map->
  [{:keys [r g b]}]
  (format "rgb(%s,%s,%s)"
          r g b))
