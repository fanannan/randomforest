(ns randomforest.gini-test
  (:require [clojure.test :refer :all]
            [randomforest.gini :refer :all]))

(deftest gini-test
  (is (= (gini [9 2 4 4 1]) 0.36)))

