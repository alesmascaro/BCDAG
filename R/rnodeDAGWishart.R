#' Draw one observation from a Normal-Inverse-Gamma distribution (internal function)
#'
#' This function performs one draw from the Multivariate-Normal-Inverse-Gamma (prior/posterior) distribution of the parameters of a Normal linear regression model.
#' Response variable is \code{node} and covariates are given by the parents of \code{node} in \code{DAG}.
#' It is implemented node-by-node in \code{rDAGWishart} to obtain draws
#' from a compatible (prior/posterior) DAG-Wishart distribution.
#'
#' @param node numerical label of the node in \code{DAG}
#' @param DAG \eqn{(q,q)} adjacency matrix of the DAG
#' @param aj common shape hyperparameter of the compatible DAG-Wishart, \eqn{a > q - 1}
#' @param U position hyperparameter of the compatible DAG-Wishart, a \eqn{(q, q)} s.p.d. matrix
#'
#' @return A list with two elements; a vector with one draw for the (vector) regression coefficient and a scalar with one draw for the conditional variance
rnodeDAGWishart <- function(node, DAG, aj, U) {
  q <- ncol(data)
  n <- nrow(data)

  j <- node
  pa <- pa(j, DAG)

  out <- list(sigmaj = 0, Lj = 0)

  if (length(pa) == 0) {
    U_jj <- U[j,j]
    out$sigmaj <- stats::rgamma(1, shape = aj/2, rate = U_jj/2)^-1
  } else {
    U_paj.j <- U[pa,j]
    invU_papa <- solve(U[pa,pa])
    U_jj <- U[j,j] - t(U_paj.j)%*%invU_papa%*%U_paj.j

    out$sigmaj <- stats::rgamma(1, shape = aj/2, rate = U_jj/2)^-1
    out$Lj <- mvtnorm::rmvnorm(1, -invU_papa%*%U_paj.j, out$sigmaj*invU_papa)
  }

  return(out)
}
