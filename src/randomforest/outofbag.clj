(ns randomforest.outofbag
  (:require [clojure.math.combinatorics :as combo]
            [randomforest.gini :as g]))

(defn out-of-bag-test [config randomforest samples]
  (throw (Exception. (str "out-of-the-bag test is not yet implemented."))))



