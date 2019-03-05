(ns patterns.emnist
  (:require [cortex.datasets.mnist :refer [assert=]]
            [clojure.java.io :as io]
            [mikera.image.core :as img]
            [clojure.data.csv :as csv]
            [inkspot.color :as ink.color]
            [clojure.string :as str])
  (:import [java.awt Color]
           [java.io File]))

(set! *warn-on-reflection* true)

(def dataset-path "./emnist/")
(def WIDTH-HEIGHT 28)

(defn- load-labels!
  []
  (with-open [rdr (io/reader (io/resource "emnist/emnist-balanced-mapping.txt"))]
    (reduce (fn [accum mapping]
              (let [[label ascii-code] (str/split mapping #" ")]
                (assoc accum
                  label (-> ascii-code
                            Integer/parseInt
                            char
                            str))))
            {}
            (line-seq rdr))))

(def label-data (memoize load-labels!))

(defn- ->character
  [label]
  (str ((label-data) label)))

(sort (vals (label-data)))

(defn- ->folder
  [character]
  (let [case-of-character (if (= character (str/upper-case character))
                            "upper"
                            "lower")]
    (str dataset-path (str/lower-case character) "-" case-of-character "/")))

(defn- ->count-file
  [character]
  (io/file (str (->folder character) "count")))

(defn- ->image-file
  [character n]
  (io/file (str (->folder character) n ".png")))

(defn- make-parents!*
  []
  (doseq [character (vals (label-data))]
    (io/make-parents (str (->folder character) ".touch"))))

(def ^:private make-parents! (memoize make-parents!*))

(defn- emnist-column->pixel
  ^Integer
  [value]
  (.getRGB ^Color
           (ink.color/coerce
             (->> (Integer/parseInt value)
                  (assoc {:r 0 :g 0 :b 0} :a)))))

(defn- image!
  [file columns]
  (assert= (* WIDTH-HEIGHT WIDTH-HEIGHT) (count columns) "Wrong column count")
  (let [tmp (img/new-image WIDTH-HEIGHT WIDTH-HEIGHT)
        pixels (img/get-pixels tmp)
        cols (vec columns)]
    (dotimes [i (* WIDTH-HEIGHT WIDTH-HEIGHT)]
      (aset pixels i (emnist-column->pixel (cols i))))
    (img/set-pixels tmp pixels)
    (img/write tmp
               file
               "png"
               :quality 1.0)))

(defn- fs-cache--character-count!*
  []
  (make-parents!)
  ;; initialize all to zero
  (doseq [character (vals (label-data))]
    (spit (->count-file character) 0))
  (doseq [[character total-count]
          (with-open [reader (io/reader (io/resource "emnist/emnist-balanced-test.csv"))]
            (reduce (fn [accum [label]]
                      (let [character (->character label)]
                        (if (accum character)
                          (update accum character inc)
                          (assoc accum character 1))))
                    {}
                    (csv/read-csv reader)))]
    (spit (->count-file character) total-count)))

(def ^:private fs-cache--character-count! (memoize fs-cache--character-count!*))

(defn- fs-cache--character-count
  [character]
  (let [fs-cached-count (->count-file character)]
    (when-not (.exists ^File fs-cached-count)
      (fs-cache--character-count!))
    (if (.exists ^File fs-cached-count)
      (Integer/parseInt (slurp fs-cached-count))
      0)))

(def character-count (memoize fs-cache--character-count))

(defn load-nth-character!*
  [character n]
  (make-parents!)
  (let [fs-cached-image (->image-file character n)]
    (when-not (.exists ^File fs-cached-image)
      (with-open [reader (io/reader (io/resource "emnist/emnist-balanced-test.csv"))]
        (loop [[[label & columns] & rows] (csv/read-csv reader)
               n-seen-so-far 0]
          (if (= (->character label)
                 character)
            (if (= n-seen-so-far n)
              (image! fs-cached-image columns)
              (recur rows (inc n-seen-so-far)))
            (recur rows n-seen-so-far))))
      fs-cached-image)))

(def ^:private load-nth-character! (memoize load-nth-character!*))

(defn random
  [character]
  (when (pos? (character-count character))
    (load-nth-character! character
                         (rand-int (character-count character)))))

(defn case-insensitive-random
  [character]
  (if-let [c (random character)]
    c
    (random
      (if (= character (str/upper-case character))
        (str/lower-case character)
        (str/upper-case character)))))

(comment
  (random "6")
  (case-insensitive-random "l"))
