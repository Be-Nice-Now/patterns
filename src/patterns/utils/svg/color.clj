(ns patterns.utils.svg.color)

(defn- ->int
  [n]
  (-> n
      float
      Math/round
      int))

(defn map->
  [{:keys [r g b]}]
  (format "rgb(%s,%s,%s)"
          r g b))

(defn ->gradient
  [start end steps]
  (let [rise (fn [start end]
               (- end start))
        run steps
        m (fn [start end]
            (/ (rise start end)
               run))
        y (fn [channel]
            (fn [step]
              (let [x (* (/ step
                            (dec steps))
                         steps)
                    b (channel start)]
                (->int
                  (+ (* (m (channel start) (channel end))
                        x)
                     b)))))]
    (map (fn [idx]
           {:r ((y :r) idx)
            :g ((y :g) idx)
            :b ((y :b) idx)})
         (range steps))))
