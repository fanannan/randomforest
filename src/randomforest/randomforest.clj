(ns randomforest.randomforest
  (:require [taoensso.nippy :as nippy]
            [randomforest.decisiontree :as dtree])
  (:import  [java.io DataInputStream DataOutputStream]))

(defn- make-sample-subsets
  "Makes sample subsets from records of learning-samples.
   The size of a sample subset is defined by sub-sampling-ratio.
   Duplication of sample records is allowed."
  [{:keys [sub-sampling-ratio num-trees] :as config} samples]
  (repeat num-trees
          (take (int (* sub-sampling-ratio (count samples)))
                (shuffle samples))))

(defn make-randomforest
  "Makes a randomforest"
  [config samples]
  (dtree/assure-featurekeys samples)
  (dtree/make-decisiontrees config (make-sample-subsets config samples)))

(defn apply-randomforest**
  "Applies a randomforest model onto a feature and returns leaves with element values."
  [randomforest feature]
  (map #(dtree/apply-decisiontree* % feature) randomforest))

(defn apply-randomforest*
  "Returns a single estimation value for the feature from the result leaves of the randomforest model."
  [config randomforest feature]
  (let [leaves (apply-randomforest** randomforest feature)
        values (flatten leaves)]
    (case (:valuation-mode config)
      :mean (double (/ (apply + values) (count values)))
      :median (first (drop (/ (count values) 2) values)))))

(defn apply-randomforest
  "Applies a randomforest model onto records (value part is not neccessary) and returns a collection of corresponding estimation values"
  [config randomforest records]
  (dtree/assure-featurekeys records)
  (map #(apply-randomforest* config randomforest (dtree/get-features %)) records))

(defn save-randomforest
  "Saves a serialized randomforest model at filepath."
  [randomforest filepath]
  (with-open [w (clojure.java.io/output-stream filepath)]
   (nippy/freeze-to-out! (DataOutputStream. w) randomforest)))

(defn load-randomforest [filepath]
  "Load a randomforest model at filepath."
  (with-open [r (clojure.java.io/input-stream filepath)]
   (nippy/thaw-from-in! (DataInputStream. r))))

