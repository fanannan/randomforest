(ns randomforest.utils
  (:require [randomforest.decisiontree :as dtree]))
(binding [*warn-on-reflection* false]
  (use '[incanter.stats :as stats]))

(defn get-actuals-and-predictions
  [predictions]
  (map (fn[x][(dtree/get-value (first x)) (second x)]) predictions))

(defn correl
  [xs ys]
  (incanter.stats/correlation xs ys))

(defn mse
  [xs ys]
  (/ (apply + (map (fn[x y](* (- x y)(- x y))) xs ys))
     (count xs)))

(defn performance
  [predictions]
  (let [[xs ys] (vec (apply map vector (get-actuals-and-predictions predictions)))]
    (assert (= (count xs)(count ys)))
    {:correlation (correl xs ys),
     :mse (mse xs ys)}))
