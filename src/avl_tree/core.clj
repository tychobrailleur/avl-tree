(ns avl-tree.core)



(defrecord Node [val left right])


(defn create []
  (->Node nil nil nil))

(defn height [tree]
  (if (nil? tree) 0
      (+ 1 (max
            (height (.left tree))
            (height (.right tree))))))


(defn balance-factor [tree]
  (if (nil? tree) 0
      (- (height (.left tree))
         (height (.right tree)))))

(defn balanced? [tree]
  (let [factor (balance-factor tree)]
    ((set '(-1 0 1)) factor)))

(defn rotate-left [tree]
  (let [right (.right tree)]
    (if right
      (->Node (.val right)
              (->Node (.val tree) (.left tree) (.left right))
              (.right right))
      tree)))

(defn rotate-right [tree]
  (let [left (.left tree)]
    (if left
      (->Node (.val left)
              (.left left)
              (->Node (.val tree) (.right left) (.right tree)))
      tree)))


(defn find-node [tree val]
  (cond
    (nil? tree) tree
    (nil? (.val tree)) tree
    (= val (.val tree)) tree
    (< val (.val tree)) (find-node (.left tree) val)
    (> val (.val tree)) (find-node (.right tree) val)))

(defn find-min [tree]
  (cond
    (nil? tree) tree
    (nil? (.left tree)) tree
    (.left tree) (find-min (.left tree))))

(defn find-max [tree]
  (cond
    (nil? tree) tree
    (nil? (.right tree)) tree
    (.right tree) (find-max (.right tree))))



(defn insert0 [tree val]
  (if (nil? tree)
    (->Node val nil nil)
    (let [cur (.val tree)]
      (if (nil? cur)
        (->Node val (.left tree) (.right tree))
        (if (= cur val)
          tree
          (if (< val cur)
            (->Node cur (insert0 (.left tree) val) (.right tree))
            (->Node cur (.left tree) (insert0 (.right tree) val))))))))


(defn remove0 [tree val]
  (cond
    (nil? tree) tree
    (nil? (.val tree)) (->Node nil
                               (remove0 (.left tree) val)
                               (remove0 (.right tree) val))
    (< val (.val tree)) (->Node (.val tree)
                                (remove0 (.left tree) val)
                                (.right tree))
    (> val (.val tree)) (->Node (.val tree)
                                (.left tree)
                                (remove0 (.right tree) val))
    (= val (.val tree))
    (cond
      (and (nil? (.left tree)) (nil? (.right tree))) nil
      ;; if node only has one child, replace node with its child.
      (nil? (.left tree)) (.left tree)
      (nil? (.right tree)) (.right tree)
      ;; when node has two children, pick successor
      :else (let [min (find-min (.right tree))]
              (->Node (.val min) (.left tree) (remove0 (.right tree) (.val min)))))))


(defn balance [tree]
  (if (not (balanced? tree))
    (if (and (balanced? (.left tree))
             (balanced? (.right tree)))
      ;; if node is first non-balanced, we need to re-balance,
      ;; new node has to be added below as a grandchild.
      (cond
        (and (> (balance-factor tree) 1) (< (balance-factor (.left tree)) 0))
        (rotate-right (->Node (.val tree) (rotate-left (.left tree)) (.right tree)))
        (and (< (balance-factor tree) -1) (> (balance-factor (.right tree)) 0))
        (rotate-left (->Node (.val tree) (.left tree) (rotate-right (.right tree))))
        (> (balance-factor tree) 1)
        (rotate-right tree)
        (< (balance-factor tree) -1)
        (rotate-left tree)
        :else tree)
      (->Node (.val tree)
              (balance (.left tree))
              (balance (.right tree))))
    tree))


(defn insert [tree val]
  (-> tree
      (insert0 val)
      balance))

(defn remove [tree val]
  (-> tree
      (remove0 val)
      balance))

(defn contains? [tree val]
  (not (nil? (find-node tree val))))


(defn tree->str [tree]
  (str "[" (.val tree) " "
       (if (nil? (.left tree))
         "nil "
         (str (tree->str (.left tree)) " "))
       (if (nil? (.right tree))
         "nil"
         (tree->str (.right tree))) "]"))


(defn display [tree]
  (println (tree->str tree)))
