#' Compute the median probability DAG model from the MCMC output
#'
#' This function computes the Median Probability DAG Model estimate (MPM) from the MCMC output of \code{learn_DAG}
#'
#' Output of \code{learn_dag} function consists of \eqn{S} draws from the joint posterior of DAGs and DAG-parameters in a zero-mean Gaussian DAG-model;
#' see the documentation of \code{learn_DAG} for more details.
#'
#' The Median Probability DAG Model estimate (MPM) is obtained by including all edges whose posterior probability exceeds 0.5.
#' The posterior probability of inclusion of \eqn{u -> v} is estimated as the frequency of DAGs visited by the MCMC which contain the directed edge \eqn{u -> v};
#' see also function \code{get_edgeprobs} and the corresponding documentation.
#'
#' @author Federico Castelletti and Alessandro Mascaro
#'
#'
#' @references F. Castelletti and A. Mascaro (2021). Structural learning and estimation of joint causal effects among network-dependent variables. \emph{Statistical Methods and Applications}, Advance publication
#' @references M.M. Barbieri and J.O. Berger (2004). Optimal predictive model selection. \emph{The Annals of Statistics} 32 870-897
#'
#' @param learnDAG_output object of class \code{bcdag}
#'
#' @return The \eqn{(q,q)} adjacency matrix of the median probability DAG model
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
#' # Produce the MPM DAG estimate
#' get_MPMdag(out_mcmc)
#'
get_MPMdag <- function(learnDAG_output) {

  if (!methods::is(learnDAG_output,"bcdag")) {
    stop("learnDAG_output must be an object of class bcdag")
  }

  edgeprobs <- get_edgeprobs(learnDAG_output)
  MPM <- round(edgeprobs)
  colnames(MPM) = rownames(MPM) = 1:ncol(MPM)
  return(MPM)
}
