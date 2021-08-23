#' Estimate posterior probabilities of edge inclusion
#'
#' @param learnDAG_output output of learn_dag()
#'
#' @return Matrix containing probability of edge inclusion
#' @export
#'
#' @examples
learn_edgeprobs <- function(learnDAG_output) {
  if (class(learnDAG_output$Graphs) == "character") {
    G <- length(learnDAG_output$Graphs)
    q <- ncol(bd_decode(learnDAG_output$Graphs[,,1]))
    Graphs <- array(dim = c(q,q,G))
    for (i in 1:G) {
      Graphs[,,i] <- bd_decode(learnDAG_output$Graphs[,,i])
    }
  } else {
    Graphs <- learnDAG_output$Graphs
  }
  edgeprobs <- apply(Graphs, c(1,2), mean)
  return(edgeprobs)
}
