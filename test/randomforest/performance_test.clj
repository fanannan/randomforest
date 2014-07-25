(ns randomforest.performance-test
  (:require [clojure.test :refer :all]
            [randomforest.utils :refer :all]))

(deftest performance-test
 (is (= (make-variations {:a [1 2] :b [3 4] :c [6 7]})
         '({:a 1, :b 3, :c 6} {:a 2, :b 3, :c 6} {:a 1, :b 4, :c 6} {:a 2, :b 4, :c 6} {:a 1, :b 3, :c 7} {:a 2, :b 3, :c 7} {:a 1, :b 4, :c 7} {:a 2, :b 4, :c 7})))
)
