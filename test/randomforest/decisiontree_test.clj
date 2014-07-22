(ns randomforest.decisiontree-test
  (:require [clojure.test :refer :all]
            [randomforest.decisiontree :refer :all]))

(deftest decisiontree-test
  ; get-features
  (is (= (get-features [{:a 1} 5]) {:a 1}))
  ; get-value
  (is (= (get-value [{:a 1} 5]) 5))
  ; get-featurekeys
  (is (= (get-featurekeys [[{:a 1 :b 2} 1]]) #{:a :b}))
  ; assure-featurekeys
  (is (assure-featurekeys [[{:a 1 :b 2} 1][{:a 2 :b 3} 2]]))
  (is (thrown? AssertionError (assure-featurekeys [[{:a 1 :b 2} 1][{:a 2 :c 3} 2]])))
  ; make-feature-key-combis
  (is (= (make-feature-key-combis 2 [[{:a 1 :b 2 :c 3} 1]]) '((:c :b) (:c :a) (:b :a))))
  ; make-featurekey-set
  ; small?
  (is (small? [1 2 3 4 5] 8))
  (is (not (small? [1 2 3 4 5] 3)))
  ; make-leaf
  ; make-node
  ; devide
  ; get-score
  (is (= (get-score [1]) 1))
  ; pick-best-division
  (is (= (pick-best-division {:verbose false} [[4][5][8][2][7]]) [2]))
  ; find-best-division-by-key
  ; find-best-division
  ; make-decisiontree
  ; make-decisiontrees
  ; apply-decisiontree*
  ; apply-decisiontree
)
