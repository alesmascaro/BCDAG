#' Find the parents of a node in a DAG (internal function)
#'
#' This function finds the set of parents of \code{node} in \code{DAG}
#'
#' @param node numerical label of the node in \code{DAG}
#' @param DAG \eqn{(q,q)} adjacency matrix of the DAG
#'
#' @return A numerical vector with the labels of the parents of \code{node} in \code{DAG}
pa <- function(node, DAG) {
  pa <- which(DAG[,node] != 0)
  return(pa)
}
