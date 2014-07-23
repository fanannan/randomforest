# randomforest

An experimental implementation of Random Forest algorithm in Clojure.

This implementation employs binary decisontrees of which nodes are build with Gini coefficient valuation. Only regression is supported and classification is under development. No post-pruning is implemented.

## Usage

See core.clj. The detail of the configuration and the data structure is described on core.clj and decisiontree.clj, respectively.

## License

Copyright © 2014 Takahiro SAWADA

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
