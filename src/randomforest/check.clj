(ns randomforest.check
  (:require [clojure.set :as s]
            [randomforest.decisiontree :as dtree]))

(defn check-keys
  "Checks if the all necessary keys in config given."
  [config]
  (let [cs (set (keys config))
        os #{:warning :verbose :type :out-of-bag-ratio :num-trees :featurekey-selection-at-node :sub-sampling-ratio :valuation-mode :num-candidate-featurekeys :max-depth :min-elements :num-threshold-trials :entropy-fn :max-entropy-score}]
    (when (not (empty? (s/difference cs os)))
      (throw (Exception. (str "extra key(s) given:" (s/difference cs os)))))
    (when (not (empty? (s/difference os cs)))
      (throw (Exception. (str "key(s) not given:" (s/difference os cs))))))
  true)

(defn check
  "Checks parameters adequacy and show warnings."
  [config samples]
  (when (:warning config)
    (let [num-samples (count samples)
          num-featurekeys (count (dtree/get-featurekeys samples))
          sqrt-num-featurekeys (Math/sqrt num-featurekeys)]

      ; out-of-bag testing
      (when (or (zero? (:out-of-bag-ratio config))(nil? (:out-of-bag-ratio config)))
        (println "out-of-bag test will not be executed."))
      (when (< (:out-of-bag-ratio config) 0)
        (throw (Exception. (str "out-of-bag ratio can not be negative number."))))

      ; sampling
      (when (> (:sub-sampling-ratio config) 0.7)
        (println ":sub-sampling-ratio may be too large."))

      ; number of decisiontrees
      (when (not (pos? (:num-trees config)))
        (throw (Exception. (str ":num-tree must be positive number."))))

      ; featurekey selection
      (when (:featurekey-selection-at-node config)
        (println "featurekey selection will be executed at every node construction. pay attention to the correlation of the featurekeys."))
      (when (case (:type config)
              :classification
              (not (< (* num-featurekeys 1/3 0.8)
                      (:num-candidate-featurekeys config)
                      (* num-featurekeys 1/3 1.25)))
              :regression
              (not (< (* sqrt-num-featurekeys 0.8)
                      (:num-candidate-featurekeys config)
                      (* sqrt-num-featurekeys 1.25))))
           (println ":num-candidate-featurekeys may not suit the number of features of the data."))
      (when (< (:num-threshold-trials config) 10) ; shoud be depent on samples?
        (println ":num-threshold-trials may be too small."))

      ; entropy function
      (when (nil? (:entropy-fn config))
        (throw (Exception. (str ":entropy-fn is not specified."))))

      ; run-time pruning
      (when (< (:min-elements config) (/ num-samples 100))
        (println ":min-elements may be too small."))
      (when (< (:max-depth config) (/ (Math/log num-samples)
                                      (:min-elements config)
                                      (Math/log 2)))
        (println ":max-depth may be too deep."))
      (when (> (:max-entropy-score config) 0.75)
        (println ":max-entropy may be too large."))

      ; evaluation
      (cond
        (and (= (type config) :classification)
             (not= (:valuation-mode config) :whole-mode)
             (not= (:valuation-mode config) :leafwise-mode))
        (throw (Exception. (str ":valuation-mode need to be :whole-mode or :leafwise-mode for classification")))
        (and (= (:type config) :regression)
             (or (= (:valuation-mode config) :whole-mode)
                 (= (:valuation-mode config) :leafwise-mode)))
        (println ":whole-mode and :leafwise-mode may not work for regression.")))))
