(ns patterns.utils
  "Clojure/edn specific utils.")

(defn veccat
  "Exactly like `concat`, just returns a vector.

   Useful for making HTML modifications in the Hiccup format."
  [& xyzs]
  (vec (apply concat xyzs)))
