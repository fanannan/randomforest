(ns randomforest.check-test
  (:require [clojure.test :refer :all]
            [randomforest.check :refer :all]))

(deftest check-test
  (is (check-keys
    {;; forest level
     :warning true, ; automatic check
     :verbose true, ; if true, showes supplimental information
     :type :regression, ; regression or classification
     :out-of-bag-ratio 0.25, ; out of bag ratio for validation
     :num-trees 5, ; number of decision trees in the randomforest
     :featurekey-selection-at-node false, ; if false, the candidate featurekeys used for a decisontree model is fixed once before starting building the decisontree. If true, the candidate featurekeys are repeatedly selected at making every node in the decisontree.
     :sub-sampling-ratio 0.7, ; sampling ratio for a decisiontree
     :valuation-mode :whole-mean, ; :whole-mean, :leafwise-mean, :whole-median or :leafwise-median, applied for all element values in the leaves (leaf-wise mean is not used)
     ;; individual tree level
     :num-candidate-featurekeys 2, ; number of featurekeys selected for a decisiontree or for a node, depending on :featurekey-selection-at-node
     :max-depth 4, ; max depth of the decisiontree
     :min-elements 2, ; minimum number of elements to make a node
     :num-threshold-trials 5, ; number of making trial nodes at node building.
     :entropy-fn nil ; entropy function
     :max-entropy-score 0.4 ; max entropy score to create new nodes.
     :parallel-search false
     })
  (is (thrown? Exception (check-keys {:dummy true})))))
