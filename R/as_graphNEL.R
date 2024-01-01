#' Transform matrix into graphNEL object
#'
#' Internal function to transform an adjacency matrix into a graphNEL object, useful for plotting
#' @param DAG Adjacency matrix of a DAG
#'
#' @return A graphNEL object
#' @export
#'
#' @examples # Randomly generate DAG
#' q <- 4; w = 0.2
#' set.seed(123)
#' DAG <- rDAG(q,w)
#' as_graphNEL(DAG)
as_graphNEL <- function(DAG) {
  graphNEL <- igraph::as_graphnel(igraph::graph_from_adjacency_matrix(DAG))
  return(graphNEL)
}
