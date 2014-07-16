# randomforests

An experimental implementation of Random Forest algorithm in Clojure.
This implementation provides only regression trees of which nodes are build with Gini coefficient valuation. When you use it for classification, you need to assign numeric values to the labels.

## Usage

See core.clj. The detail of the data structure is described on decisiontree.clj.

When :featurekey-selection-at-node is false, the candidate featurekeys used for a decisontree model is fixed before starting building the decisontree. If true, the candidate featurekeys are repeatedly selected at making every node in the decisontree.

## License

Copyright © 2014 Takahiro SAWADA

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
