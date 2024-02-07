#' Compute posterior probabilities of edge inclusion from the MCMC output
#'
#' This function computes the posterior probability of inclusion for each edge \eqn{u -> v} given the MCMC output of \code{learn_DAG};
#'
#' Output of \code{learn_dag} function consists of \eqn{S} draws from the joint posterior of DAGs and DAG-parameters in a zero-mean Gaussian DAG-model;
#' see the documentation of \code{learn_DAG} for more details.
#'
#' The posterior probability of inclusion of \eqn{u -> v} is estimated as the frequency of DAGs visited by the MCMC which contain the directed edge \eqn{u -> v}.
#' Posterior probabilities are collected in a \eqn{(q,q)} matrix with \eqn{(u,v)}-element representing the estimated posterior probability
#' of edge \eqn{u -> v}.
#'
#' @author Federico Castelletti and Alessandro Mascaro
#'
#'
#' @references F. Castelletti and A. Mascaro (2021). Structural learning and estimation of joint causal effects among network-dependent variables. \emph{Statistical Methods and Applications}, Advance publication.
#'
#' @param learnDAG_output object of class \code{bcdag}
#'
#' @return A \eqn{(q,q)} matrix with posterior probabilities of edge inclusion
#' @export
#'
#' @examples # Randomly generate a DAG and the DAG-parameters
#' q = 8
#' w = 0.2
#' set.seed(123)
#' DAG = rDAG(q = q, w = w)
#' outDL = rDAGWishart(n = 1, DAG = DAG, a = q, U = diag(1, q))
#' L = outDL$L; D = outDL$D
#' Sigma = solve(t(L))%*%D%*%solve(L)
#' # Generate observations from a Gaussian DAG-model
#' n = 200
#' X = mvtnorm::rmvnorm(n = n, sigma = Sigma)
#' # Run the MCMC (Set S = 5000 and burn = 1000 for better results)
#' out_mcmc = learn_DAG(S = 500, burn = 100, a = q, U = diag(1,q)/n, data = X, w = 0.1,
#'                      fast = TRUE, save.memory = FALSE)
#' # Compute posterior probabilities of edge inclusion
#' get_edgeprobs(out_mcmc)
#'
get_edgeprobs <- function(learnDAG_output) {

  if (!methods::is(learnDAG_output,"bcdag")) {
    stop("learnDAG_output must be an object of class bcdag")
  }

  type <- attributes(learnDAG_output)$type
  input <- attributes(learnDAG_output)$input


  if (type == "compressed and collapsed" | type == "compressed") {
    S <- length(learnDAG_output$Graphs)
    q <- sqrt(length(bd_decode(learnDAG_output$Graphs[1])))
    Graphs <- array(dim = c(q,q,S))
    for (i in 1:S) {
      Graphs[,,i] <- bd_decode(learnDAG_output$Graphs[i])
    }
  } else {
    Graphs <- learnDAG_output$Graphs
  }
  edgeprobs <- apply(Graphs, c(1,2), mean)
  colnames(edgeprobs) = rownames(edgeprobs) = 1:ncol(edgeprobs)
  return(edgeprobs)
}
