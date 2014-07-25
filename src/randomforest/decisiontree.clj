(ns randomforest.decisiontree
  (:require [clojure.math.combinatorics :as combo]
            [randomforest.gini :as g]))
; data
;  :- a map with class-map (if classification) and records
;     class-map is for the classification labels
;  {:class-map {:a 1, :b 2, :c 3}, :records [[{:a 0.1, :b 2.3, :c 3.6} 3] ...]}
;
; samples or records
;  :- a vector of records
;
; record
;  :- a vector consiting of a feature map and the corresponding value
;  [{:f1 1, :f2 2} 0.7]
;
; divion info
;  :- a vector of score (gini coefficient), featurekey, thereshold and child samples
;  [score featurekey threshold child1 child2]
;
; leaf
;  :- a map with element values
;  {:type  :leaf,
;   :value [0.2 0.3 0.8 0.9]} ; leaf element values
;
; node
;  :- a map with a branch tree dividing condition and its child nodes
;  {:type  :node,
;   :divider [:f1 0.48],      ; feature key and thresshold value
;   :score score,             ; performance of the branch division (gini coefficient)
;   :value [ {...} {...} ]}   ; vector of child binary node maps and/or leaf map(s)

(defn get-features
  "Returns feature map part of a record"
  [record]
  (first record))

(defn get-value
  "Returns value part of a record"
  [record]
  (second record))

(defn get-featurekeys
  "Gets features of the learning-samples,
   with assuring that all records of the samples have the same features."
  [samples]
  (set (keys (ffirst samples))))

(defn assure-featurekeys
  "Assures that all records of the samples have the same features."
  [samples]
  (dorun (map #(assert (= (get-featurekeys samples)
                          (set (keys (first %)))))
              samples))
  true)

(defn make-feature-key-combis
  ; this functions is not efficient. should be rewritten.
  [num-candidate-featurekeys samples]
  (combo/combinations (get-featurekeys samples)
                      num-candidate-featurekeys))

(defn make-featurekey-sets
  "Creates feature sets for individual decision trees."
  [{:keys [num-trees num-candidate-featurekeys] :as config} samples]
  (map set
       (take num-trees
         (shuffle (make-feature-key-combis num-candidate-featurekeys samples)))))

(defn- make-featurekey-set
  "Creates feature set for a node."
  [{:keys [num-candidate-featurekeys] :as config} samples]
  (set (first (shuffle (make-feature-key-combis num-candidate-featurekeys samples)))))

(defn small? [samples min-elements]
  "Checks if the number of samples are below minimum number of elements."
  (< (count samples) min-elements))

(defn make-leaf
  "Makes a leaf of a decisiontree, holding all corresponding elements"
  [config samples]
  ;(when (:verbose config)
  ;  (println "making a leaf:" (count samples)))
  {:type :leaf,
   :value (map get-value samples)})

(declare make-decisiontree) ; for recursion

(defn make-node
  "Makes a branch node of a decisiontree."
  [config depth featurekeys [score featurekey threshold child1 child2]]
  ;(when (:verbose config)
  ;  (println "making a node:" featurekey score))
  {:type :node,
   :divider [featurekey threshold], ; a vector of featurekey and a thereshold value
   :score score,
   :value [(make-decisiontree config depth featurekeys child1)
           (make-decisiontree config depth featurekeys child2)]})

(defn divide
  "Divide a sample records into two child sample record sets at the position
  and by the featurekey and then returns a vector of average gini coefficient
  as score, thredhold value and the child sample records (= division-info)."
  [config position featurekey sorted-samples]
  (let [child1     (take position sorted-samples),
        child2     (drop position sorted-samples),
        threshold  (/ (+ (featurekey (get-features (last child1))),
                         (featurekey (get-features (first child2)))) 2),
        entropy-fn (get config :entropy-fn (fn[_](throw (Exception. "entropy function not defined."))))
        e1         (entropy-fn (map get-value child1)),
        e2         (entropy-fn (map get-value child2)),
        score      (/ (+ (* e1 (count child1)) (* e2 (count child2)))
                      (count sorted-samples))]
    [score featurekey threshold child1 child2]))

(defn get-score
  "Returns score from a division-info"
  [division-info]
  (first division-info))

(defn pick-best-division
  "Returns a division-info with the least average gini coefficient."
  [config division-infos]
  ;(when (:verbose config)
  ;  (println "scores: " (map get-score division-infos))
  ;  (println "min score: " (apply min (map get-score division-infos))))
  (let [best-value (apply min (map get-score division-infos))]
    (first (filter #(= best-value (get-score %)) division-infos))))

(defn find-best-division-by-key
  "Finds a best division by the featurekey among num-feature-selection trials
   with randomly selected division point."
  [{:keys [num-threshold-trials] :as config} featurekey samples]
  (let [sorted (sort-by #(featurekey (get-features %)) samples)]
    (pick-best-division config
      (map #(divide config % featurekey sorted)
           (take num-threshold-trials (shuffle (range 1 (count samples))))))))

(defn find-best-division
  "Finds a best division among num-feature-selection trials with each featukey."
  [config featurekeys samples]
  (let [fks (if (:featurekey-selection-at-node config)
              (make-featurekey-set config samples)
              featurekeys)]
    (pick-best-division config
      (map #(find-best-division-by-key config % samples) fks))))

(defn make-decisiontree
  "Makes a decisiontree."
  [{:keys [max-depth min-elements max-entropy-score] :as config} depth featurekeys samples]
  (if (or (>= depth max-depth)
          (small? samples min-elements))
      (make-leaf config samples)
      (let [division-info (find-best-division config featurekeys samples)]
        (if (< (get-score division-info) max-entropy-score)
          (make-node config (inc depth) featurekeys division-info)
          (make-leaf config samples)))))

(defn make-decisiontrees
  "Makes decision trees"
  [config sample-subsets]
  (map #(make-decisiontree config 1 %1 %2)
       (if (:featurekey-selection-at-node config)
         (repeat (count sample-subsets) nil)
         (make-featurekey-sets config (first sample-subsets)))
       sample-subsets))

(defn apply-decisiontree*
  "Applies a decisiontree model onto a feature and returns a leaf with element values"
  [decisiontree feature]
  (case (:type decisiontree)
    :leaf (:value decisiontree)
    :node (let [[featurekey threshold] (:divider decisiontree)]
            (apply-decisiontree*
              ((if (< (featurekey feature) threshold) first second)
               (:value decisiontree))
              feature))))

(defn apply-decisiontree
  "Applies a decisiontree model onto records (value part is not neccessary) and returns corresponding leaves with element values"
  [decisiontree records]
  (map #(apply-decisiontree* decisiontree (get-features %)) records))
