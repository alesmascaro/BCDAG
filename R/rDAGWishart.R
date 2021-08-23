
#' Random samples from a DAG-Wishart distribution with G&H hyperparameters
#'
#' @param n
#' @param DAG
#' @param a
#' @param U
#'
#' @return
#' @export
#'
#' @examples

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
