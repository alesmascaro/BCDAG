#' MCMC proposal distribution (internal function)
#'
#' This function implements a proposal distribution for the MCMC scheme of \code{learn_DAG}.
#' Given an input \code{DAG}, it first builds the set of all DAGs which can be obtained by applying a local move
#' (insertion, deletion or reversal of one edge) to \code{DAG},
#' that is the set of direct successors of \code{DAG};
#' next, it randomly draws one candidate (proposed) DAG from the so-obtained set.
#' Finally, the set of direct successors of the proposed DAG is constructed.
#' The function returns: the proposed DAG, the type of operator applied to \code{DAG} to obtain the proposed DAG
#' (with value 1 if insertion, 2 if deletion, 3 if reversal),
#' the nodes involved in the local move, the number of direct successors of \code{DAG} and of the proposed DAG.
#' If \code{fast = TRUE} the two numbers of direct successors are approximated by the number of possible operators that can be applied to the DAGs
#' (equal for the two graphs)
#'
#' @param DAG Adjacency matrix of the current DAG
#' @param fast boolean, if \code{TRUE} an approximate proposal is implemented
#' @return A list containing the \eqn{(q,q)} adjacency matrix of the proposed DAG, the type of applied operator (with values in \eqn{{1,2,3}}), the numerical labels of the nodes involved in the move, the integer number of direct successors of \code{DAG} and of the proposed DAG
propose_DAG <- function(DAG, fast) {
  A <- DAG
  q <- ncol(A)
  A_na <- A
  diag(A_na) <- NA

  # Define the set of possible operations
  # The cardinality of O will change depending on how many edges are present in the DAG

  id_set = c()
  dd_set = c()
  rd_set = c()

  ## set of nodes for id
  set_id = which(A_na == 0, TRUE)
  if(length(set_id) != 0){
    id_set = cbind(1, set_id)
  }

  ## set of nodes for dd
  set_dd = which(A_na == 1, TRUE)
  if(length(set_dd != 0)){
    dd_set = cbind(2, set_dd)
  }

  ## set of nodes for rd
  set_rd = which(A_na == 1, TRUE)
  if(length(set_rd != 0)){
    rd_set = cbind(3, set_rd)
  }

  O = rbind(id_set, dd_set, rd_set)

  # Sample one random operator and verify it produces a DAG

  if (fast == FALSE) {
    proposed.opcardvec <- vector(length = nrow(O))
    for (i in 1:nrow(O)) {
      proposed.opcardvec[i] <- gRbase::is.DAG(operation(O[i,1], DAG, O[i,2:3]))
    }
    proposed.opcard <- sum(proposed.opcardvec)
    i <- sample(which(proposed.opcardvec), 1)
    A_next <- operation(O[i,1], A, O[i,2:3])
    current.opcard <- get_opcard(A_next)
  } else {
    repeat {
      i <- sample(nrow(O), 1)
      A_next <- operation(O[i,1], A, O[i,2:3])
      verify <- gRbase::is.DAG(A_next)

      if (verify == TRUE) {
        break
      }
    }
    proposed.opcard <- nrow(O)
    current.opcard <- nrow(O)
  }

  op.type <- O[i,1]
  if (op.type == 3) {
    op.node <- O[i,-1]
  } else {
    op.node <- O[i,3]
  }

  return(list(proposedDAG = A_next, op.type = op.type, op.node = op.node,
              current.opcard = current.opcard, proposed.opcard = proposed.opcard))
}
