(ns randomforest.gini)

(defn gini
  "Returns Gini coefficient of the samples"
  [samples]
  (let [sorted (sort samples)
        cum-sorted (reductions + sorted)
        ctr (count samples)
        cum (apply + samples)
        ave (/ cum ctr)
        cum-ave (map #(* ave %) (range 1 (inc ctr)))
        gini-dividee (apply + (map #(- %1 %2) cum-ave cum-sorted))
        gini-divider (* ctr cum 0.5)]
        (/ gini-dividee gini-divider)))

;(gini [9 2 4 4 1])
; → 0.36
