# BCDAG 1.1.1

## New functions
* New `confint()` method for objects of class `bcdagCE`

# BCDAG 1.1.0

## New functions
* New `print()` and `plot` methods for objects of class `bcdag`;
* New object class `bcdagCE`, containing the output of function `get_causaleffect()`, and associated `print()`, `summary()` and `plot` methods;
* New function `get_neighboringDAGs()` to obtain all the DAGs that can be reached from a given DAG through an edge addition, removal or reversal;
* New function `as_graphNEL()` easily converting an adjacency matrix into a `graphNEL` object;

## Minor improvements and fixes
* Faster matrix inversion in `DW_nodelml()` and internal function `rnodeDAGWishart()`via Cholesky decomposition;
* Faster matrix crossproduct via `crossprod(X)` in `learn_DAG()`;
* `get_diagnostics()` now allows for different graphical settings and selection of specific nodes of the DAG;
* Bug correction in `summary()` method for object of class `bcdag`;


# BCDAG 1.0

* First release of the package;
