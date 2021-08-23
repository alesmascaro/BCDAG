#' Median Probability DAG estimation
#'
#' @param learnDAG_output output of learn_dag()
#'
#' @return Adjacency matrix of Median probability DAG
#' @export
#'
#' @examples
learn_MPMdag <- function(learnDAG_output) {
  edgeprobs <- learn_edgeprobs(learnDAG_output)
  MPM <- round(edgeprobs)
  return(MPM)
}
