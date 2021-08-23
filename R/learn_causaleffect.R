#' Estimate (joint) causal effects from DAG learning procedure
#'
#' @param targets vector, targets of intervention
#' @param response numeric, response variable
#' @param learnDAG_output list, output of learn_DAG()
#' @param BMA if TRUE, a single vector containing a Bayesian model averaging
#'   estimate of the causal effect is returned
#'
#' @return Either a vector containing BMA causal effect estimate, or a matrix with G rows containing different
#'   causal effects estimates (one for each DAG in the mcmc)
#' @export
#'
#' @examples
learn_causaleffect <- function(learnDAG_output, targets, response, BMA = FALSE) {

  if (class(learnDAG_output$Graphs) == "character") {
    G <- length(learnDAG_output$Graphs)
    q <- ncol(bd_decode(learnDAG_output$Graphs[,,1]))
    Graphs <- array(dim = c(q,q,G))
    L <- array(dim = c(q,q,G))
    D <- array(dim = c(q,q,G))
    for (i in 1:G) {
      Graphs[,,i] <- bd_decode(learnDAG_output$Graphs[,,i])
      Ls[,,i] <- bd_decode(learnDAG_output$L[,,i])
      Ds[,,i] <- bd_decode(learnDAG_output$D[,,i])
    }
  } else {
    G <- dim(learnDAG_output$Graphs)[3]
    Graphs <- learnDAG_output$Graphs
    Ls <- learnDAG_output$L
    Ds <- learnDAG_output$D
  }

  effects <- matrix(0, ncol = length(targets), nrow = n)
  for (i in 1:G) {
    causaleffects[i,] <- causaleffect(targets, response, Graphs[,,i], Ls[,,i], D[,,i])
  }
  if (BMA == FALSE) {
    return(causaleffects)
  } else {
    BMA_causaleffect <- colMeans(causaleffects)
    return(BMA_causaleffect)
  }
}
