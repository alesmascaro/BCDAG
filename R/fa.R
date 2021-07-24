#' Learn the family of a node given a DAG
#'
#' @param node
#' @param DAG
#'
#' @return fa
#' @export
#'
#' @examples

fa <- function(node, DAG) {
  # DAG <- as(DAG, "matrix")
  pa <- which(DAG[,node] != 0)
  fa <- c(node, pa)
  return(fa)
}
