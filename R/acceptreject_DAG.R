#' Accept/reject the proposed DAG given the current DAG (internal function)
#'
#' This function computes the Metropolis Hastings acceptance rate for \code{proposedDAG} given \code{currentDAG}
#' and the accepts/rejects \code{proposedDAG} based on the Metropolis Hastings acceptance probability.
#' \code{proposedDAG} is a direct successor of \code{currentDAG}, which was obtained by applying an operator of type InsertD, DeleteD or ReverseD to \code{currentDAG}.
#' The two DAGs only differ by one edge \eqn{u -> v} which has been inserted/deleted/reversed in \code{currentDAG}.
#'
#' @param tXX \eqn{(q,q)} matrix \eqn{X'X} with \eqn{X} the \eqn{(n,q)} data matrix
#' @param n number of observations (rows) in the data matrix \eqn{X}
#' @param currentDAG \eqn{(q,q)} adjacency matrix of current DAG
#' @param proposedDAG \eqn{(q,q)} adjacency matrix of proposed DAG
#' @param node nodes \eqn{u} and \eqn{v} involved in the modified edge \eqn{u -> v}
#' @param op.type the type of operator applied to \code{currentDAG} to obtain \code{proposedDAG}
#' @param a shape hyperparameter of the DAG Wishart prior
#' @param U position hyperparameter of the DAG Wishart prior
#' @param w prior probability of edge inclusion
#' @param current.opcard number of direct successors of \code{currentDAG}
#' @param proposed.opcard number of direct successors of \code{proposedDAG}
#' @noRd
#' @keywords internal
#'
#' @return A Boolean indicating whether \code{proposedDAG} has been accepted (\code{TRUE}) or not (\code{FALSE})

acceptreject_DAG <- function(tXX, n, currentDAG, proposedDAG, node, op.type,
                             a, U, w, current.opcard, proposed.opcard) {
  logprior.ratios <- c(log(w/(1-w)), log((1-w)/w), log(1))
  logprior.ratio <- logprior.ratios[op.type]

  logproposal.ratio <- log(current.opcard) - log(proposed.opcard)


  if (op.type != 3) {
    current_lml <- DW_nodelml(node, currentDAG, tXX, n, a, U)
    proposed_lml <- DW_nodelml(node, proposedDAG, tXX, n, a, U)
  } else {
    current_lml <- DW_nodelml(node[1], currentDAG, tXX, n, a, U) +
      DW_nodelml(node[2], currentDAG, tXX, n, a, U)
    proposed_lml <- DW_nodelml(node[1], proposedDAG, tXX, n, a, U) +
      DW_nodelml(node[2], proposedDAG, tXX, n, a, U)
  }


  acp.ratio <- min(0, proposed_lml - current_lml + logprior.ratio +
                     logproposal.ratio)
  is.accepted <- log(stats::runif(1)) < acp.ratio

  return(is.accepted)
}
