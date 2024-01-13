#' Transform adjacency matrix into graphNEL object
#'
#' Function to transform an adjacency matrix into a graphNEL object.
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
  q <- ncol(DAG)
  nodes <- as.character(1:q)
  ft <- which(DAG != 0, T)
  graphNEL <- graph::ftM2graphNEL(ft, V = nodes, edgemode = "directed")
  return(graphNEL)
}
