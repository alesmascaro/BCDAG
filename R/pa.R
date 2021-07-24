#' Learn the parents of a node in a DAG
#'
#' @param node
#' @param DAG
#'
#' @return pa
#' @export
#'
#' @examples
pa <- function(node, DAG) {
  # DAG <- as(DAG, "matrix")
  pa <- which(DAG[,node] != 0)
  return(pa)
}
