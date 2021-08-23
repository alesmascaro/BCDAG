
#' A simple function to perform an add/remove/reverse edge operation on a DAG
#'
#' @param op The type of operation performed: op == 1 corresponds to adding an edge, op == 2 corresponds to removing an edge, op == 3 corresponds to reversing an edge
#' @param A The adjacency matrix of the DAG on which the operation is performed
#' @param nodes The nodes on which the operation is to be performed
#'
#' @return The adjacency matrix of the modified DAG
#' @export
#'
#' @examples

operation <- function(op, A, nodes) {
  x <- nodes[1]
  y <- nodes[2]

  if(op == 1) {
    A[x,y] = 1
    return(A)
  }

  if(op == 2) {
    A[x,y] = 0
    return(A)
  }

  if(op == 3) {
    A[x,y] = 0
    A[y,x] = 1
    return(A)
  }
}
