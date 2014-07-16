(ns randomforest.decisiontree
  (:require [clojure.math.combinatorics :as combo]
            [randomforest.gini :as g]))

; samples
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

(defn- get-featurekeys
  "Gets features of the learning-samples,
   with assuring that all records of the samples have the same features."
  [samples]
  (set (keys (ffirst samples))))

(defn assure-featurekeys
  "Assures that all records of the samples have the same features."
  [samples]
  (dorun (map #(assert (= (get-featurekeys samples)
                          (set (keys (first %))))) samples)))

(defn make-feature-key-combis
  ; this functions is not efficient. should be rewritten.
  [num-features samples]
  (combo/combinations (get-featurekeys samples)
                      num-features))

(defn make-featurekey-sets
  "Creates feature sets for individual decision trees."
  [{:keys [num-trees num-features] :as config} samples]
  (map set
       (take num-trees
         (shuffle (make-feature-key-combis num-features samples)))))

(defn- make-featurekey-set
  "Creates feature set for a node."
  [{:keys [num-features] :as config} samples]
  (set (first (shuffle (make-feature-key-combis num-features samples)))))

(defn small? [samples min-elements]
  "Checks if the number of samples are below minimum number of elements."
  (< (count samples) min-elements))

(defn make-leaf
  "Makes a leaf of a decisiontree, holding all corresponding elements"
  [samples]
  (println "making a leaf:" (count samples))
  {:type :leaf,
   :value (map get-value samples)})

(declare make-decisiontree) ; for recursion

(defn make-node
  "Makes a branch node of a decisiontree."
  [config depth featurekeys [score featurekey threshold child1 child2]]
  (println "making a node:" featurekey score)
  {:type :node,
   :divider [featurekey threshold], ; a vector of featurekey and a thereshold value
   :score score,
   :value [(make-decisiontree config depth featurekeys child1)
           (make-decisiontree config depth featurekeys child2)]})

(defn divide
  "Divide a sample records into two child sample record sets at the position
  and by the featurekey and then returns a vector of average gini coefficient
  as score, thredhold value and the child sample records (= division-info)."
  [position featurekey sorted-samples]
  (let [child1    (take position sorted-samples),
        child2    (drop position sorted-samples),
        ;_ (println (count child1) " / " (count child2))
        ;_ (println (last child1) " / " (first child2))
        threshold (/ (+ (featurekey (get-features (last child1))),
                        (featurekey (get-features (first child2)))) 2),
        ;_ (println threshold)
        gini1     (g/gini (map get-value child1)),
        gini2     (g/gini (map get-value child2)),
        ;_ (println gini1 " / " gini2)
        score     (/ (+ (* gini1 (count child1)) (* gini2 (count child2)))
                     (count sorted-samples))]
    [score featurekey threshold child1 child2]))

(defn- get-score
  "Returns score from a division-info"
  [division-info]
  (first division-info))

(defn pick-best-division
  "Returns a division-info with the least average gini coefficient."
  [division-infos]
  ;(println "scores: " (map get-score division-infos))
  ;(println "min: " (apply min (map get-score division-infos)))
  (let [best-value (apply min (map get-score division-infos))]
    (first (filter #(= best-value (get-score %)) division-infos))))

(defn find-best-division-by-key
  "Finds a best division by the featurekey among num-feature-selection trials
   with randomly selected division point."
  [{:keys [num-threshold-trials]} featurekey samples]
  (let [sorted (sort-by #(featurekey (get-features %)) samples)]
    (pick-best-division
      (map #(divide % featurekey sorted)
           (take num-threshold-trials (shuffle (range 1 (count samples))))))))

(defn find-best-division
  "Finds a best division among num-feature-selection trials with each featukey."
  [config featurekeys samples]
  (let [fks (if (:featurekey-selection-at-node config)
              (make-featurekey-set config samples)
              featurekeys)]
    (pick-best-division
      (map #(find-best-division-by-key config % samples) fks))))

(defn make-decisiontree
  "Makes a decisiontree."
  [{:keys [max-depth min-elements] :as config} depth featurekeys samples]
  (if (or (>= depth max-depth)
          (small? samples min-elements))
      (make-leaf samples)
      (let [division-info (find-best-division config featurekeys samples)]
        (make-node config (inc depth) featurekeys division-info))))

(defn make-decisiontrees
  "Makes decision trees"
  [config sample-subsets]
  (map #(make-decisiontree config 1 %1 %2)
       (if (:featurekey-selection-at-node config)
         (repeat (count sample-subsets) nil)
         (make-featurekey-sets config sample-subsets))
       sample-subsets))

(defn apply-decisiontree*
  "Applies a decisiontree model onto a feature and returns a leaf with element values"
  [decisiontree feature]
  (case (:type decisiontree)
    :leaf (:value decisiontree)
    :node (let [[featurekey threshold] (:divider decisiontree)]
            ;(println featurekey threshold feature)
            (apply-decisiontree*
              ((if (< (featurekey feature) threshold) first second)
               (:value decisiontree))
              feature))))

(defn apply-decisiontree
  "Applies a decisiontree model onto records (value part is not neccessary) and returns corresponding leaves with element values"
  [decisiontree records]
  (map #(apply-decisiontree* decisiontree (get-features %)) records))
