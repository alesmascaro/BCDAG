#' Sample 1 observation for a single node of a DAG from a DAG-Wishart distribution
#'
#' @param node The node for which
#' @param DAG
#' @param a
#' @param U
#'
#' @return
#' @export
#'
#' @examples

rnodeDAGWishart <- function(node, DAG, aj, U) {
  q <- ncol(data)
  n <- nrow(data)

  j <- node
  pa <- pa(j, DAG)

  out <- list(sigmaj = 0, Lj = 0)

  if (length(pa) == 0) {
    U_jj <- U[j,j]
    out$sigmaj <- rgamma(1, shape = aj/2, rate = U_jj/2)^-1
  } else {
    U_paj.j <- U[pa,j]
    invU_papa <- solve(U[pa,pa])
    U_jj <- U[j,j] - t(U_paj.j)%*%invU_papa%*%U_paj.j

    out$sigmaj <- rgamma(1, shape = aj/2, rate = U_jj/2)^-1
    out$Lj <- mvtnorm::rmvnorm(1, -invU_papa%*%U_paj.j, out$sigmaj*invU_papa)
  }

  return(out)
}
