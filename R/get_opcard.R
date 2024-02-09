#' Find the direct successors DAGs of an input DAG (internal function)
#'
#' @param DAG The input DAG
#'
#' @return The cardinality of the set of DAGs that can be reached by addition, removal or reversal of one edge.
#' @noRd
#' @keywords internal

get_opcard <- function(DAG) {
  A <- DAG
  q <- ncol(A)
  A_na <- A
  diag(A_na) <- NA

  # Define the set of possible operations!

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
  op.cardvec <- vector(length = nrow(O))
  for (i in 1:nrow(O)) {
    op.cardvec[i] <- gRbase::is.DAG(operation(O[i,1], DAG, O[i,2:3]))
  }
  op.card <- sum(op.cardvec)
  return(op.card)
}
