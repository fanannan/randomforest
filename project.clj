(defproject randomforest "0.1.0-SNAPSHOT"
  :description "An experimental implementation of randomforest in Clojure"
  :url "https://github.com/fanannan/randomforest"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/math.combinatorics "0.0.7"]
                 [com.taoensso/nippy "2.6.3"]
                 [org.clojure/clojure "1.6.0"]
                 [org.clojure/data.csv "0.1.2"]
                 [incanter "1.5.5"]
                 ]
  :main randomforest.core)
