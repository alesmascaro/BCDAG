#' Compute node-marginal likelihoods of a DAG model (internal function)
#'
#' This function computes the log-marginal likelihood of the conditional distribution of variable \code{node}
#' given its parents in \code{DAG} under a DAG-Wishart prior on the DAG model-parameters
#'
#' @param node numerical label of \eqn{node} in \code{DAG}
#' @param DAG \eqn{(q,q)} adjacency matrix of \code{DAG}
#' @param tXX \eqn{(q,q)} matrix \eqn{X'X} with \eqn{X} the \eqn{(n,q)} data matrix
#' @param n number of observations (rows) in the data matrix \eqn{X}
#' @param a shape hyperparameter of the DAG Wishart prior
#' @param U position hyperparameter of the DAG Wishart prior
#'
#' @return The logarithm of the marginal likelihood of \code{node}
#' @export
DW_nodelml <- function(node, DAG, tXX, n, a, U) {
  j <- node
  pa <- pa(j, DAG)
  q <- ncol(tXX)

  a.star <- (a+length(pa)-q+1)


  Upost <- U + tXX

  if (length(pa) == 0) {
    U_jj <- U[j,j]
    Upost_jj <- Upost[j,j]

    prior.normcost <- -lgamma(a.star/2) + a.star/2*log(U_jj/2)
    post.normcost <- -lgamma(a.star/2 + n/2) + (a.star/2 + n/2)*log(Upost_jj/2)

  } else {
    U_paj.j <- U[pa,j]
    U_jj <- U[j,j] - t(U_paj.j)%*%solve(U[pa,pa])%*%U_paj.j
    Upost_paj.j <- Upost[pa,j]
    Upost_jj <- Upost[j,j] - t(Upost_paj.j)%*%solve(Upost[pa,pa])%*%Upost_paj.j

    prior.normcost <- -lgamma(a.star/2) + a.star/2*log(U_jj/2) + 0.5*log(det(as.matrix(U[pa,pa])))
    post.normcost <- -lgamma(a.star/2 + n/2) + (a.star/2 + n/2)*log(Upost_jj/2) + 0.5*log(det(as.matrix(Upost[pa,pa])))

  }

  nodelml <- -n/2*log(2*pi) + prior.normcost - post.normcost

  return(nodelml)
}
