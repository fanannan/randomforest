(ns randomforest.performance
  (:require [clojure.math.combinatorics :as combo]
            [randomforest.decisiontree :as dtree]
            [randomforest.randomforest :as forest]))
(binding [*warn-on-reflection* false]
  (use '[incanter.stats :as stats]))

(defn average
  "Returns the average."
  [xs]
  (if (empty? xs) Double/NaN
    (double (/ (apply + xs)(count xs)))))

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

(defn true-positive
  [label predictions]
  (count (filter (fn[[a p]](= a p label)) predictions)))

(defn false-positive
  [label predictions]
  (count (filter (fn[[a p]](and (= p label))(not= a label)) predictions)))

(defn false-negative
  [label predictions]
  (count (filter (fn[[a p]](and (not= p label))(= a label)) predictions)))

(defn precision
  [label predictions]
  (let [tp (true-positive label predictions)]
    (double (/ tp (+ tp (false-positive label predictions))))))

(defn recall
  [label predictions]
  (let [tp (true-positive label predictions)]
    (double (/ tp (+ tp (false-negative label predictions))))))

(defn f-measure
  [label predictions]
  (let [p (precision label predictions)
        r (recall label predictions)]
    (if (or (Double/isNaN p)(Double/isNaN r)(zero? (+ r p)))
      Double/NaN
      (/ (* 2 r p)(+ r p)))))

(defn performance
  "Returns a map with correlation and MSE (in case of regression) or
   precision, recall and f-measure (in case of classification)
   of the actual values and the predictions."
  [config predictions]
  (let [ap (get-actuals-and-predictions predictions),
        [xs ys] (vec (apply map vector ap))]
    (case (:type config)
      :regression
        {:correlation (correl xs ys),
         :mse (mse xs ys)}
      :classification
      (let [labels (set xs)
            p (map #(precision % ap) labels),
            r (map #(recall % ap) labels),
            f (map #(f-measure % ap) labels)]
        {:precision (zipmap labels p),
         :recall (zipmap labels r),
         :f-measure (zipmap labels f),
         :average-precision (average p),
         :average-recall (average r),
         :average-f-measure (average f)}))))

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
    (performance config (map (fn[r p][r p]) validation-samples ps))))

(defn grid-search*
  "Builds randomforests num-trials times and validates their performance with all the combinations of the parameters which is given in the hash map m. It is executed parallely if :parallel-search is true."
  [config m num-trials learning-samples validation-samples]
  (let [vs (make-variations m)]
    ((if (:parallel-search config) pmap map)
       (fn[c]
           (when (:verbose config)
             (println "validating: " c))
           [c (repeatedly num-trials
                          #(validate (conj config c)
                                     learning-samples validation-samples))])
         vs)))

(defn aggregate
  "Calculates the average performance of the randomforests with the same parameter set."
  [ms]
  ;({:a 1}{:a 2}) -> {:a 1.5} ; :a-stdev xxx}
  (let [ks (keys (first ms))
        vs (map (fn[k](map #(get % k) ms)) ks)]
    (zipmap ks (map #(if (every? number? %) (average %) %) vs))))

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
      :correlation (last sorted),
      :average-precision (last sorted),
      :average-recall (last sorted),
      :average-f-measure (last sorted))))
