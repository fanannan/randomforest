(ns randomforest.randomforest
  (:require [taoensso.nippy :as nippy]
            [randomforest.check :as check]
            [randomforest.decisiontree :as dtree]
            [randomforest.outofbag :as oob])
  (:import  [java.io DataInputStream DataOutputStream]))

(defn make-sample-subsets
  "Makes num-trees of subsets from records of samples.
   The size of a subset is defined by sub-sampling-ratio.
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

(defn build
  [config data]
  (let [class-map (get data :class-map nil)
        shuffled (shuffle (get data :records []))
        oob-ratio (get config :out-of-bag-ratio 0.0)
        num-oob (* oob-ratio (count shuffled))
        oob (take num-oob shuffled)
        samples (drop num-oob shuffled)]
    (when (get config :warning true)
      (check/check config samples))
    (let [rf (make-randomforest config samples)]
      (when (pos? oob-ratio)
        ;(oob/out-of-bag-test config rf oob)
      ))))

(defn apply-randomforest**
  "Applies a randomforest model onto a feature and returns leaves with element values."
  [randomforest feature]
  (map #(dtree/apply-decisiontree* % feature) randomforest))

(defn evaluate
  "Returns a single estimation value for the feature, with the result leaves of the randomforest model."
  [config leaves]
  (let [all-leaf-elements (flatten leaves)
        mean (fn[xs](double (/ (apply + xs) (count xs))))
        median (fn[xs](nth (sort xs) (int (/ (count xs) 2))))
        mode (fn[xs](ffirst (sort-by val > (frequencies xs))))]
    (case (:valuation-mode config)
      :leafwise-mean (mean (map mean leaves))
      :whole-mean (mean all-leaf-elements)
      :leafwise-median (median (map median leaves))
      :whole-median (median all-leaf-elements)
      :leafwise-mode (mode (map mode leaves))
      :whole-mode (mode all-leaf-elements))))

(defn apply-randomforest*
  "Returns a single estimation value for the feature, with the result leaves of the randomforest model."
  [config randomforest feature]
  (evaluate config (apply-randomforest** randomforest feature)))

(defn apply-randomforest
  "Applies a randomforest model onto records (value part is not neccessary) and returns a collection of corresponding estimation values"
  [config randomforest records]
  (dtree/assure-featurekeys records)
  (map #(apply-randomforest* config randomforest (dtree/get-features %)) records))

(defn reverse-map
  [m]
  (if (nil? m)
    nil
    (into {} (map (fn[[k v]][v k]) m))))

(defn predict
  "Predicts value or class with the given randomforest"
  [config randomforest data]
  (let [class-map (get data :class-map nil)
        reversed-class-map (reverse-map class-map)
        records (shuffle (get data :records []))
        predictions (apply-randomforest config randomforest records)]
    (map (fn[r p][r (if (nil? reversed-class-map)
                      p
                      (get reversed-class-map p))])
         records predictions)))

(defn save-randomforest
  "Saves a serialized randomforest model at filepath."
  [randomforest filepath]
  (with-open [w (clojure.java.io/output-stream filepath)]
   (nippy/freeze-to-out! (DataOutputStream. w) randomforest)))

(defn load-randomforest
  "Load a randomforest model at filepath."
  [filepath]
  (with-open [r (clojure.java.io/input-stream filepath)]
   (nippy/thaw-from-in! (DataInputStream. r))))
