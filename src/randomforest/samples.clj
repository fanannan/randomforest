(ns randomforest.samples
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn read-csv [input-file-path]
  (with-open [in-file (io/reader input-file-path)]
    (doall (csv/read-csv in-file))))

(defn get-classification-records [filepath]
  (let [all (read-csv filepath)
        header (map keyword (first all))
        contents (map #(map read-string %)(rest all))
        raw-data (map #(zipmap header %) contents)
        cleaned-data (map #(dissoc % :id) raw-data)
        kinds (set (map :kind raw-data))
        class-map (zipmap kinds (range))
        data (map (fn[m](update-in m [:kind] #(get class-map %))) cleaned-data)
        records (mapv (fn[m][(dissoc m :kind) (:kind m)]) data)]
    {:class-map class-map, ; for converting name labels from/to number labels
     :records records}))

(def classification-data (get-classification-records "./resources/iris.csv"))

(def regression-data ; an artificial linear samples
  {:class-map nil, ; for converting name labels from/to number labels
   :records [[{	:f1	5	:f2	0	:f3	0	:f4	4	}	44	]
             [{	:f1	7	:f2	1	:f3	2	:f4	8	}	71	]
             [{	:f1	4	:f2	6	:f3	5	:f4	9	}	101	]
             [{	:f1	8	:f2	9	:f3	8	:f4	4	}	185	]
             [{	:f1	0	:f2	0	:f3	0	:f4	5	}	5	]
             [{	:f1	9	:f2	7	:f3	4	:f4	2	}	137	]
             [{	:f1	7	:f2	2	:f3	4	:f4	8	}	82	]
             [{	:f1	5	:f2	3	:f3	6	:f4	2	}	75	]
             [{	:f1	3	:f2	5	:f3	5	:f4	0	}	74	]
             [{	:f1	3	:f2	9	:f3	9	:f4	1	}	151	]
             [{	:f1	9	:f2	4	:f3	9	:f4	2	}	130	]
             [{	:f1	4	:f2	1	:f3	6	:f4	6	}	49	]
             [{	:f1	0	:f2	2	:f3	7	:f4	8	}	32	]
             [{	:f1	2	:f2	9	:f3	4	:f4	8	}	105	]
             [{	:f1	0	:f2	1	:f3	3	:f4	2	}	10	]
             [{	:f1	0	:f2	7	:f3	9	:f4	2	}	100	]
             [{	:f1	0	:f2	0	:f3	3	:f4	4	}	4	]
             [{	:f1	1	:f2	9	:f3	3	:f4	3	}	83	]
             [{	:f1	5	:f2	8	:f3	3	:f4	4	}	108	]
             [{	:f1	7	:f2	8	:f3	9	:f4	5	}	173	]
             [{	:f1	0	:f2	1	:f3	3	:f4	9	}	17	]
             [{	:f1	7	:f2	0	:f3	0	:f4	8	}	64	]
             [{	:f1	3	:f2	1	:f3	1	:f4	2	}	32	]
             [{	:f1	5	:f2	4	:f3	6	:f4	9	}	93	]
             [{	:f1	6	:f2	1	:f3	3	:f4	8	}	64	]
             [{	:f1	4	:f2	9	:f3	8	:f4	4	}	153	]
             [{	:f1	5	:f2	3	:f3	3	:f4	2	}	66	]
             [{	:f1	0	:f2	8	:f3	1	:f4	0	}	48	]
             [{	:f1	9	:f2	1	:f3	0	:f4	4	}	81	]
             [{	:f1	5	:f2	7	:f3	1	:f4	0	}	82	]
             [{	:f1	5	:f2	9	:f3	6	:f4	4	}	143	]
             [{	:f1	9	:f2	0	:f3	1	:f4	9	}	81	]
             [{	:f1	1	:f2	9	:f3	0	:f4	4	}	57	]
             [{	:f1	8	:f2	9	:f3	9	:f4	6	}	196	]
             [{	:f1	2	:f2	5	:f3	5	:f4	8	}	74	]
             [{	:f1	1	:f2	8	:f3	3	:f4	0	}	72	]
             [{	:f1	1	:f2	2	:f3	5	:f4	0	}	28	]
             [{	:f1	5	:f2	8	:f3	4	:f4	5	}	117	]
             [{	:f1	1	:f2	8	:f3	2	:f4	9	}	73	]
             [{	:f1	1	:f2	0	:f3	1	:f4	1	}	9	]
             [{	:f1	9	:f2	8	:f3	1	:f4	8	}	128	]
             [{	:f1	5	:f2	4	:f3	7	:f4	0	}	88	]
             [{	:f1	0	:f2	8	:f3	6	:f4	8	}	96	]
             [{	:f1	8	:f2	5	:f3	8	:f4	6	}	135	]
             [{	:f1	6	:f2	3	:f3	0	:f4	5	}	68	]
             [{	:f1	5	:f2	5	:f3	7	:f4	2	}	102	]
             [{	:f1	2	:f2	8	:f3	3	:f4	2	}	82	]
             [{	:f1	0	:f2	1	:f3	6	:f4	7	}	18	]
             [{	:f1	4	:f2	1	:f3	3	:f4	8	}	48	]
             [{	:f1	2	:f2	3	:f3	0	:f4	6	}	37	]
             ]})

