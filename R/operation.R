#' Perform local moves given a DAG (internal function)
#'
#' This function locally modifies a DAG by inserting (\code{op = 1}), deleting (\code{op = 2}) or reversing (\code{op = 3}) an edge between two \code{nodes}
#'
#' @param op numerical type in \eqn{{1,2,3}} of the operator applied to \code{DAG}
#' @param A \eqn{(q,q)} adjacency matrix of the input DAG
#' @param nodes numerical labels of nodes on which the operator is applied, a \eqn{(2,1)} vector
#' @noRd
#' @keywords internal
#'
#' @return The \eqn{(q,q)} adjacency matrix of the modified DAG
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
