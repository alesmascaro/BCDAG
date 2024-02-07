#' Random samples from a compatible DAG-Wishart distribution
#'
#'
#' This function implements a direct sampling from a compatible DAG-Wishart distribution with parameters \code{a} and \code{U}.
#'
#' Assume the joint distribution of random variables \eqn{X_1, \dots, X_q} is zero-mean Gaussian with covariance matrix Markov w.r.t. a Directed Acyclic Graph (DAG).
#' The allied Structural Equation Model (SEM) representation of a Gaussian DAG-model allows to express the covariance matrix as a function of the (Cholesky) parameters \eqn{(D,L)},
#' collecting the regression coefficients and conditional variances of the SEM.
#'
#' The DAG-Wishart distribution (Cao et. al, 2019) with shape hyperparameter \eqn{a = (a_1, ..., a_q)} and position hyperparameter \eqn{U} (a s.p.d. \eqn{(q,q)} matrix) provides a conjugate prior for parameters \eqn{(D,L)}.
#' In addition, to guarantee compatibility among Markov equivalent DAGs (same marginal likelihood), the default choice (here implemented) \eqn{a_j = a + |pa(j)| - q + 1} \eqn{(a > q - 1)}, with \eqn{|pa(j)|} the number of parents of node \eqn{j} in the DAG,
#' was introduced by Peluso and Consonni (2020).
#'
#'
#' @param n number of samples
#' @param DAG \eqn{(q, q)} adjacency matrix of the DAG
#' @param a common shape hyperparameter of the compatible DAG-Wishart, \eqn{a > q - 1}
#' @param U position hyperparameter of the compatible DAG-Wishart, a \eqn{(q, q)} s.p.d. matrix
#'
#' @return A list of two elements: a \eqn{(q,q,n)} array collecting \eqn{n} sampled matrices \eqn{L} and a \eqn{(q,q,n)} array collecting \eqn{n} sampled matrices \eqn{D}
#' @export
#'
#' @author Federico Castelletti and Alessandro Mascaro
#'
#'
#' @references F. Castelletti and A. Mascaro (2021). Structural learning and estimation of joint causal effects among network-dependent variables. \emph{Statistical Methods and Applications}, Advance publication.
#' @references X. Cao, K. Khare and M. Ghosh (2019). Posterior graph selection and estimation consistency for high-dimensional Bayesian DAG models. \emph{The Annals of Statistics} 47 319-348.
#' @references S. Peluso and G. Consonni (2020). Compatible priors for model selection of high-dimensional Gaussian DAGs. \emph{Electronic Journal of Statistics} 14(2) 4110 - 4132.
#'
#' @examples # Randomly generate a DAG on q = 8 nodes with probability of edge inclusion w = 0.2
#' q = 8
#' w = 0.2
#' set.seed(123)
#' DAG = rDAG(q = q, w = w)
#' # Draw from a compatible DAG-Wishart distribution with parameters a = q and U = diag(1,q)
#' outDL = rDAGWishart(n = 5, DAG = DAG, a = q, U = diag(1, q))
#' outDL
rDAGWishart <- function(n, DAG, a, U) {

  q <- ncol(DAG)
  ajs <- sapply(1:q, function(j) a+sum(DAG[,j]==1)-q+1)

  L.array <- array(0, dim = c(q,q,n))
  D.array <- array(0, dim = c(q,q,n))

  for (i in 1:n) {
    params <- lapply(1:q, function(j) rnodeDAGWishart(j, DAG, ajs[j], U))

    sigmas <- sapply(1:q, function(x) params[[x]]$sigmaj)
    L <- lapply(1:q, function(x) params[[x]]$Lj)

    D.array[,,i] <- diag(sigmas)
    for (j in 1:q) {
      whc <- which(DAG[,j] == 1)
      L.array[whc,j,i] <- as.numeric(L[[j]])
    }
    diag(L.array[,,i]) <- 1
  }

  if (n == 1) {
    D.array <- D.array[,,1]
    L.array <- L.array[,,1]
  }

  return(list(D = D.array, L = L.array))
}
