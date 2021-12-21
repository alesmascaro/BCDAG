
# Overview

**BCDAG** (**B**ayesian **C**ausal **DAG**) is a package for causal (DAG) structure learning and causal effect estimation from observational Gaussian data in the Bayesian setting.

The methodology implemented has been presented in [Castelletti, F. \& Mascaro, A. (2021). *Structural learning and estimation of joint causal effects among network-dependent variables*, Statistical Methods \& Applications, 1-26](https://link.springer.com/article/10.1007/s10260-021-00579-1)

Currently, the package is still in an early stage of development. External contributions are more than welcome. 

# Installation

The package is not available on CRAN. However, you can install it directly from this repository.

``` r
# install.packages("devtools")
devtools::install_github("alesmascaro/BCDAG")
```

# How to use

The workflow of the package consists of two sequential steps: causal structure learning, performed through the function `learn_DAG()` and causal effect estimation, performed through the function `get_causaleffect()`. For a more detailed description of the inner mechanisms of these two functions, we refer the reader to the vignette.

## Example

Before using the two main functions of the package, we generate some data:

``` r
# Randomly generate a DAG and the DAG-parameters
q = 8
w = 0.2
set.seed(123)
DAG = rDAG(q = q, w = w)
outDL = rDAGWishart(n = 1, DAG = DAG, a = q, U = diag(1, q))
L = outDL$L; D = outDL$D
Sigma = solve(t(L))%*%D%*%solve(L)
n = 200
  # Generate observations from a Gaussian DAG-model
X = mvtnorm::rmvnorm(n = n, sigma = Sigma)
```

Then, we can use the function `learn_DAG()` to perform causal structure learning from the generated observational dataset:

``` r
# Run the MCMC
out = learn_DAG(S = 5000, burn = 1000, a = q, U = diag(1,q)/n, data = X, w = w)
```
Then, we can compute the BMA estimate of the causal effect of a hard intervention on a set of nodes on a response variable using `get_causaleffect()`:

``` r
# the causal effect on node 1 of an intervention on {3,4}
out |>
  get_causaleffect(targets = c(3,4), response = 1, BMA = TRUE)
```

# Authors

- Alessandro Mascaro, University of Milano-Bicocca, a.mascaro3@campus.unimib.it
- Federico Castelletti, Catholic University of the Sacred Heart, Milan

