(ns patterns.utils.paths
  "Various path generation helpers, etc.")

(defn control-points
  [max-stroke-width
   {:keys [width height]}
   {[x1 y1] :start
    [x2 y2] :end}]
  (let [[x1--control-point y1--control-point]
        (cond
          (zero? x1) [max-stroke-width y1]
          (zero? y1) [x1 max-stroke-width]
          (= width x1) [(- width max-stroke-width) y1]
          (= height y1) [x1 (- height max-stroke-width)])
        [x2--control-point y2--control-point]
        (cond
          (zero? x2) [max-stroke-width y2]
          (zero? y2) [x2 max-stroke-width]
          (= width x2) [(- width max-stroke-width) y2]
          (= height y2) [x2 (- height max-stroke-width)])]
    {:start--point
     [x1 y1]
     :start--control-point [x1--control-point y1--control-point]
     :mid-control-point
     (cond
       (= x1--control-point
          x2--control-point)
       [x1--control-point
        (int (/ (+ y1--control-point
                   y2--control-point)
                2))]

       (= y1--control-point
          y2--control-point)
       [(int (/ (+ x1--control-point
                   x2--control-point)
                2))
        y1--control-point]

       :else
       [x1--control-point y2--control-point])
     :center
     [(/ width 2)
      (/ height 2)]
     :end--control-point [x2--control-point y2--control-point]
     :end--point
     [x2 y2]}))

(defn straight-line-fn
  [max-stroke-width details points]
  (let [{:keys [start--point
                start--control-point
                center
                end--control-point
                end--point]} (control-points max-stroke-width details points)
        d (format (str "M %s %s"
                       " L %s %s"
                       " L %s %s"
                       " L %s %s"
                       " L %s %s")
                  (first start--point)
                  (second start--point)
                  (first start--control-point)
                  (second start--control-point)
                  (first center)
                  (second center)
                  (first end--control-point)
                  (second end--control-point)
                  (first end--point)
                  (second end--point))]
    [:g {}
     [:g {:id "backfill"}
      [:path {:d d}]]
     [:g {:id "midfill"}
      [:path {:d d}]]
     [:path {:d d}]]))

(defn curved-line-fn
  [max-stroke-width details points]
  (let [{:keys [start--point
                start--control-point
                center
                end--control-point
                end--point]} (control-points max-stroke-width details points)
        d (format (str "M %s %s"
                       " Q %s %s, %s %s"
                       " T %s %s, %s %s"
                       " L %s %s")
                  (first start--point)
                  (second start--point)
                  (first start--control-point)
                  (second start--control-point)
                  (first start--control-point)
                  (second start--control-point)
                  (first center)
                  (second center)
                  (first end--control-point)
                  (second end--control-point)
                  (first end--point)
                  (second end--point))]
    [:g {}
     [:g {:id "backfill"}
      [:path {:d d}]]
     [:g {:id "midfill"}
      [:path {:d d}]]
     [:path {:d d}]]))

(defn turned-line-fn
  [max-stroke-width details points]
  (let [{:keys [start--point
                start--control-point
                end--control-point
                end--point]} (control-points max-stroke-width details points)
        d (format (str "M %s %s"
                       " L %s %s"
                       " L %s %s"
                       " L %s %s")
                  (first start--point)
                  (second start--point)
                  (first start--control-point)
                  (second start--control-point)
                  (first end--control-point)
                  (second end--control-point)
                  (first end--point)
                  (second end--point))]
    [:g {}
     [:g {:id "backfill"}
      [:path {:d d}]]
     [:g {:id "midfill"}
      [:path {:d d}]]
     [:path {:d d}]]))

(defn single-bend-line-fn
  [max-stroke-width details points]
  (let [{:keys [start--point
                start--control-point
                mid-control-point
                end--control-point
                end--point]} (control-points max-stroke-width details points)
        d (format (str "M %s %s"
                       " L %s %s"
                       " L %s %s"
                       " L %s %s"
                       " L %s %s")
                  (first start--point)
                  (second start--point)
                  (first start--control-point)
                  (second start--control-point)
                  (first mid-control-point)
                  (second mid-control-point)
                  (first end--control-point)
                  (second end--control-point)
                  (first end--point)
                  (second end--point))]
    [:g {}
     [:g {:id "backfill"}
      [:path {:d d}]]
     [:g {:id "midfill"}
      [:path {:d d}]]
     [:path {:d d}]]))
