#' Find the family of a node in a DAG (internal function)
#'
#' This function finds the family (union of \code{node} and its parents) of \code{node} in \code{DAG}
#'
#' @param node numerical label of the node in \code{DAG}
#' @param DAG \eqn{(q,q)} adjacency matrix of the DAG
#'
#' @return A numerical vector with the labels of the family of \code{node} in \code{DAG}
fa <- function(node, DAG) {
  pa <- which(DAG[,node] != 0)
  fa <- c(node, pa)
  return(fa)
}
