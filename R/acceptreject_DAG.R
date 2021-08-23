#' Accept/Reject a proposed DAG
#'
#' @param X Matrix of centered data
#' @param tXX Empirical covariance matrix (given centered data)
#' @param currentDAG As the name suggests
#' @param proposedDAG As the name suggests
#' @param node Nodes that differ between the two DAGs
#' @param op.type The operation performed on the current DAG to obtain the proposed DAG
#' @param a hyperparameter
#' @param U hyperparameter
#' @param w hyperparameter
#'
#' @return A Boolean indicating whether the proposed DAG has been accepted or not
#' @export
#'
#' @examples

acceptreject_DAG <- function(X, tXX, currentDAG, proposedDAG, node, op.type,
                             a, U, w, current.opcard, proposed.opcard) {
  logprior.ratios <- c(log(w/(1-w)), log((1-w)/w), log(1))
  logprior.ratio <- logprior.ratios[op.type]

  # if (fast == FALSE) {
  #   proposed.opcard <- get_opcard(proposedDAG)
  #   current.opcard <- get_opcard(currentDAG)
  #   logproposal.ratio <- log(current.opcard/proposed.opcard)
  # }

  logproposal.ratio <- log(current.opcard) - log(proposed.opcard)


  if (op.type != 3) {
    current_mll <- DW_nodemll(node, currentDAG, X, tXX, a, U)
    proposed_mll <- DW_nodemll(node, proposedDAG, X, tXX, a, U)
  } else {
    current_mll <- DW_nodemll(node[1], currentDAG, X, tXX, a, U) +
      DW_nodemll(node[2], currentDAG, X, tXX, a, U)
    proposed_mll <- DW_nodemll(node[1], proposedDAG, X, tXX, a, U) +
      DW_nodemll(node[2], proposedDAG, X, tXX, a, U)
  }


  acp.ratio <- min(0, proposed_mll - current_mll + logprior.ratio +
                     logproposal.ratio)
  is.accepted <- log(runif(1)) < acp.ratio

  return(is.accepted)
}
