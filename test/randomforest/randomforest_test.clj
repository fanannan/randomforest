(ns randomforest.randomforest-test
  (:require [clojure.test :refer :all]
            [randomforest.randomforest :refer :all]))


(deftest randomforest-test
  ; make-sample-subsets
  ; make-randomforest
  ; build
  ; apply-randomforest**
  ; evaluate
  (is (= (evaluate {:valuation-mode :leafwise-mean}   [[2 4][4 5 6]]) 4.0))
  (is (= (evaluate {:valuation-mode :whole-mean}      [[2 2 4][4 6 12]]) 5.0))
  (is (= (evaluate {:valuation-mode :leafwise-median} [[1 2 4][3][5 8 12]]) 3))
  (is (= (evaluate {:valuation-mode :whole-median}    [[1 2 4][3][5 8 12]]) 4))
  (is (= (evaluate {:valuation-mode :leafwise-mode}   [[1 1 1 2][1 3 3][1 3 3]]) 3))
  (is (= (evaluate {:valuation-mode :whole-mode}      [[1 1 1 2][1 3 3][1 3 3]]) 1))
  ; apply-randomforest*
  ; apply-randomforest
  ; reverse-map
  (is (= {:a 1 :b 2} (reverse-map {1 :a 2 :b})))
  ; predict
  (let [rf '{:type :node,
             :divider [:f2 3],
             :score 0.2,
             :value
             [{:type :node,
               :divider [:f1 2],
               :score 0.3,
               :value
               [{:type :node,
                 :divider [:f1 0],
                 :score 0.3,
                 :value
                 [{:type :leaf, :value (5 4)}
                  {:type :leaf, :value (10 17 18 32 9 28)}]}]}]}
        filepath "/tmp/temp.rf"]
  ; save-randomforest
  ; load-randomforest
  (save-randomforest rf filepath)
  (is (= rf (load-randomforest filepath)))))
