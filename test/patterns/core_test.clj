(ns patterns.core-test
  (:require [clojure.test :refer :all]
            [patterns.hiccup :as hiccup]
            [patterns.core :as patterns]))

(deftest render--string-html
  (testing "empty svg"
    (let [empty-svg [:svg {}]]
      (is (= empty-svg
             (-> [:svg {}]
                 patterns/render
                 hiccup/svg->hiccup))))))
