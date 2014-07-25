(ns randomforest.performance
  (:require [clojure.math.combinatorics :as combo]
            [randomforest.decisiontree :as dtree]
            [randomforest.randomforest :as forest]))
(binding [*warn-on-reflection* false]
  (use '[incanter.stats :as stats]))

(defn get-actuals-and-predictions
  "Gets actual values given in the samples and predictions with the features and the randomforest."
  [predictions]
  (map (fn[x][(dtree/get-value (first x)) (second x)]) predictions))

(defn correl
  "Calculates correlation of the actual values and the predictions."
  [xs ys]
  (try
    (incanter.stats/correlation xs ys)
    (catch Exception ex Double/NaN)))

(defn mse
  "Calculates mean squared error (MSE) of the actual values and the predictions."
  [xs ys]
  (if (empty? xs)
    Double/NaN
    (double
      (/ (apply + (map (fn[x y](* (- x y)(- x y))) xs ys))
        (count xs)))))

(defn performance
  "Returns a map with correlation and MSE of the actual values and the predictions."
  [predictions]
  (let [[xs ys] (vec (apply map vector (get-actuals-and-predictions predictions)))]
    (assert (= (count xs)(count ys)))
    {:correlation (correl xs ys), :mse (mse xs ys)}))

(defn make-variations
  "Makes all combinations of the grid parameters."
  [m]
  (let [featurekeys (keys m)]
    (map #(zipmap featurekeys %)
          (apply combo/cartesian-product (map m featurekeys)))))

(defn validate
  "Builds a randomforest with the learning-samples and then validates it with the validation-samples."
  [config learning-samples validation-samples]
  (let [rf (forest/make-randomforest config learning-samples)
        ps (forest/apply-randomforest config rf validation-samples)]
    (performance (map (fn[r p][r p]) validation-samples ps))))

(defn grid-search*
  "Builds randomforests num-trials times and validates their performance with all the combinations of the parameters which is given in the hash map m. It is executed parallely if :parallel-search is true."
  [config m num-trials learning-samples validation-samples]
  (let [vs (make-variations m)]
    ((if (:parallel-search config) pmap map)
       (fn[c]
           (when (:verbose config)
             (println "validating performance of: " c))
           [c (repeat num-trials
                      (validate (conj config c)
                                learning-samples validation-samples))])
         vs)))

(defn aggregate
  "Calculates the average performance of the randomforests with the same parameter set."
  [ms]
  ;({:a 1}{:a 2}) -> {:a 1.5} ; :a-stdev xxx}
  (let [ks (keys (first ms))
        vs (map (fn[k](map #(get % k) ms)) ks)]
    (zipmap ks (map (fn[xs](if (empty? xs) Double/NaN
                             (/ (apply + xs)(count xs)))) vs))))

(defn grid-search
  "Executes a grid-search"
  [config m num-trials learning-samples validation-samples]
  (let [rs (grid-search* config m num-trials learning-samples validation-samples)]
    (map (fn[[c ps]][c (aggregate ps)]) rs)))

(defn get-best-params-by-grid-search
  "Returns the best performing parameter set."
  [config m target num-trials learning-samples validation-samples]
  (let [gr (grid-search config m num-trials learning-samples validation-samples)
        cf (fn[d](or (Double/isNaN d)
                     (= Double/POSITIVE_INFINITY d)
                     (= Double/NEGATIVE_INFINITY d)))
        filtered (remove #(cf (get (second %) target)) gr)
        sorted (sort-by #(get (second %) target) < filtered)]
    (case target
      :mse (first sorted),
      :correlation (last sorted))))
