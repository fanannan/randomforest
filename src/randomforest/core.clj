(ns randomforest.core
  (:use [clojure.pprint :as pp]
        [randomforest.decisiontree :as dtree]
        [randomforest.randomforest :as forest]
        [randomforest.gini :as gini]
        [randomforest.performance :as performance]
        [randomforest.outofbag :as oob]
        [randomforest.samples :as samples]))


; sample configs

(def regression-config
  {;; forest level
   :warning true, ; automatic check
   :verbose true ; if true, showes supplimental information
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
   :entropy-fn gini/gini ; entropy function
   :max-entropy-score 0.4 ; max entropy score to create new nodes.
   ;; grid search
   :parallel-search true ; seach best combination of the parameters parallely
   })

; classification mode is still under development
#_(def classification-config
  {;; forest level
   :warning true, ; automatic check
   :verbose true ; if true, showes supplimental information
   :type :classification, ; regression or classification
   :out-of-bag-ratio 0.25, ; out of bag ratio for validation
   :num-trees 5, ; number of decision trees in the randomforest
   :featurekey-selection-at-node false, ; if false, the candidate featurekeys used for a decisontree model is fixed once before starting building the decisontree. If true, the candidate featurekeys are repeatedly selected at making every node in the decisontree.
   :sub-sampling-ratio 0.7, ; sampling ratio for a decisiontree
   :valuation-mode :whole-mode, ; :whole-mode or :leafwise-mode, applied for all element values in the leaves (leaf-wise mode is not used)
   ;; individual tree level
   :num-candidate-featurekeys 2, ; number of featurekeys selected for a decisiontree or for a node, depending on :featurekey-selection-at-node
   :max-depth 4, ; max depth of the decisiontree
   :min-elements 2, ; minimum number of elements to make a node
   :num-threshold-trials 5, ; number of making trial nodes at node building.
   :entropy-fn gini/gini ; entropy function
   :max-entropy-score 0.4 ; max entropy score to create new nodes.
   ;; grid search
   :parallel-search true ; seach best combination of the parameters parallely
   })


(defn -main [& args]
  (let [rs samples/regression-data
        rsr (:records rs)
        ;cs samples/classification-data
        ;t (dtree/make-decisiontree regression-config 1 #{:f1 :f2 :f4} rsr)
        ;rr (forest/make-randomforest regression-config rsr)
        rb  (forest/build regression-config rs)
        ;cr (forest/build classification-config cs)
        rp (forest/predict regression-config rb rs)
        ]
    ;(forest/save-randomforest rb "/tmp/r.f")
    ;(pp/pprint (forest/apply-randomforest regression-config (forest/load-randomforest "/tmp/r.f") rsr))
    (pp/pprint rb)
    (pp/pprint rp)
    (pp/pprint (performance/performance rp))
    ; grid search
    (pp/pprint (performance/get-best-params-by-grid-search
                              regression-config,
                              {:max-depth [4 5],
                               :min-elements [5 8],
                               :num-threshold-trials [5 10],
                               :max-entropy-score [0.25 0.5],
                               :valuation-mode [:whole-mean, :leafwise-mean, :whole-median, :leafwise-median]}
                              :correlation, ;:mse
                              10
                              (take (* (count rsr) 0.7) rsr)
                              (drop (* (count rsr) 0.7) rsr)))
    ;(pp/pprint cr)
    ;(pp/pprint (forest/predict classification-config cr cs))
    (when (:parallel-search regression-config)
      (shutdown-agents))
    ))
