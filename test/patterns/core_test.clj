(ns patterns.core-test
  (:require [clojure.test :refer :all]
            [patterns.core :as patterns]
            [patterns.hiccup :as hiccup]))

(deftest render--string-html
  (testing "empty svg"
    (let [empty-svg [:svg {}]]
      (is (= empty-svg
             (-> [:svg {}]
                 patterns/render
                 hiccup/svg->hiccup))))))
