(ns avl-tree.core-test
  (:require [clojure.test :refer :all]
            [avl-tree.core :refer :all]))

(deftest test-height
  (testing "empty returns 0"
    (is (= 0 (height nil))))
  (testing "single node returns 1"
    (is (= 1 (height (create)))))
  (testing "complex height"
    (is (= 4 (height (->Node 4 (->Node 2 (->Node 1 (->Node 0 nil nil) nil) (->Node 3 nil nil)) nil))))))

(deftest test-balance-factor
  (testing "empty returns 0"
    (is (= 0 (balance-factor nil))))
  (testing "single node returns 0"
    (is (= 0 (balance-factor (create)))))
  (testing "balance factor of a non-balanced tree"
    (is (= 3 (balance-factor (->Node 4 (->Node 2 (->Node 1 (->Node 0 nil nil) nil) (->Node 3 nil nil)) nil)))))
  (testing "balance factor of a non-balanced tree"
    (is (= -1 (balance-factor (->Node 4 (->Node 3 nil nil) (->Node 5 nil (->Node 6 nil nil))))))))
