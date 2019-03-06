(ns patterns.utils.io.zip
  (:require [clojure.java.io :as io])
  (:import [java.util.zip ZipEntry ZipFile]
           [java.io Reader]))

(defn entries
  [^ZipFile file]
  (enumeration-seq
    (.entries file)))

(defn ls
  [^ZipFile file]
  (vec (for [e (entries file)]
         (.getName ^ZipEntry e))))

(defn ^Reader reader
  [^ZipFile file entry]
  (->> (entries file)
       (filter (fn [^ZipEntry e]
                 (= entry
                    (.getName e))))
       first
       (.getInputStream file)
       io/reader))

(defn ^ZipFile path->file
  [^String n]
  (ZipFile. n))

(defn ^ZipFile resource->file
  [n]
  (-> (if (string? n)
        (io/resource n)
        n)
      io/file
      (.getAbsolutePath)
      path->file))
