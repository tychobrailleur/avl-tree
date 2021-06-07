(ns avl-tree.example
  (:require [avl-tree.core :as avl]))


(defn example []
  (-> (avl/create)
      (avl/insert 3)
      (avl/insert 1)
      (avl/insert 2)
      (avl/insert 4)
      (avl/insert 6)
      (avl/insert 5)))


(defn -main [& args]
  (let [ex (example)]
    (avl/display ex)
    (avl/display (avl/remove ex 4))))
