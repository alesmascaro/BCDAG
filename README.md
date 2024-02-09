
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Overview

**BCDAG** (**B**ayesian **C**ausal **DAG**) is a package for Bayesian
DAG structure learning and causal effect estimation from observational
Gaussian data.

The methodology implemented has been presented in [Castelletti, F. &
Mascaro, A. (2021). *Structural learning and estimation of joint causal
effects among network-dependent variables*, Statistical Methods &
Applications,
1-26](https://link.springer.com/article/10.1007/s10260-021-00579-1)

# Installation

The package can be installed executing the following code:

``` r
# install.packages("devtools")
devtools::install_github("alesmascaro/BCDAG")
```

# How to use

The workflow of the package consists of two sequential steps: causal
structure learning, performed through the function `learn_DAG()` and
causal effect estimation, performed through the function
`get_causaleffect()`. For a more detailed description of the inner
mechanisms of these two functions, we refer the reader to the vignettes.

## Example

Before using the two main functions of the package, we generate some
data:

``` r
library(BCDAG)
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

We can use the function `learn_DAG()` to perform causal structure
learning from the generated observational dataset:

``` r
# Run the MCMC
out = learn_DAG(S = 5000, burn = 1000, a = q, U = diag(1,q)/n, data = X, w = w)
```

Next, we can compute the BMA estimate of the causal effect on a response
variable consequent to a hard intervention on a set of nodes by using
`get_causaleffect()`:

``` r
# the causal effect on node 1 of an intervention on {3,4}
out |>
  get_causaleffect(targets = c(3,4), response = 1)
```

The three steps here implemented are detailed in the vignettes [1 -
Random data generation from Gaussian
DAG-models](https://alesmascaro.github.io/BCDAG/bcdag_generatedata.html),
[2 - MCMC scheme for posterior inference of Gaussian DAG models: the
`learn_DAG()`
function](https://alesmascaro.github.io/BCDAG/bcdag_learnDAG.html) and
[3 - Elaborate on the output of `learn_DAG()` using get\_
functions](https://alesmascaro.github.io/BCDAG/bcdag_getfamily.html)

# Authors

- Alessandro Mascaro, Departament d’Economia i Empresa, Universitat
  Pompeu Fabra, Barcelona, <alessandro.mascaro@upf.edu>
- Federico Castelletti, Department of Statistical sciences, Università
  Cattolica del Sacro Cuore, Milan, <federico.castelletti@unicatt.it>

<!-- badges: start -->

[![R-CMD-check](https://github.com/alesmascaro/BCDAG/workflows/R-CMD-check/badge.svg)](https://github.com/alesmascaro/BCDAG/actions)
<!-- badges: end -->
