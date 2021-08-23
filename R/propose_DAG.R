
#' An internal function that randomly modifies a DAG and checks that the resulting DAG is a DAG
#'
#' @param DAG Adjacency matrix of the current DAG
#'
#' @return A list containing the proposed DAG, the type of operation performed, the nodes on which the operation has been performed and the cardinality of all the possible operations
#' @export
#'
#' @examples

propose_DAG <- function(DAG, fast) {
  A <- DAG
  q <- ncol(A)
  A_na <- A
  diag(A_na) <- NA

  # Define the set of possible operations!
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

  # Actually sample one random operation and verify it produces a DAG

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
