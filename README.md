# playground
Assorted functions, including

* A racket function for generating random connected graphs. These graphs are slightly biased, since they are simply random graphs (generated using the renyi model) to which a simple path has been superimposed in order to ensure connectedness (random-connected-graph.rkt).
* A small set of haskell definitions that can be used to generate typed expressions (ccg.hs).

Currently as Draft:
* Haskell implementation of a function that enumerates the subtrees of a tree whose depth is at most "d" and have at most "c" children (get_subtrees.hs). ToDo: implement using streams.
