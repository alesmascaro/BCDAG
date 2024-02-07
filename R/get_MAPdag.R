#' Compute the maximum a posteriori DAG model from the MCMC output
#'
#' This function computes the maximum a posteriori DAG model estimate (MAP) from the MCMC output of \code{learn_DAG}
#'
#' Output of \code{learn_dag} function consists of \eqn{S} draws from the joint posterior of DAGs and DAG-parameters in a zero-mean Gaussian DAG-model;
#' see the documentation of \code{learn_DAG} for more details.
#'
#' The Maximum A Posteriori (MAP) model estimate is defined as the DAG visited by the MCMC with the highest associated posterior probability.
#' Each DAG posterior probability is estimated as the frequency of visits of the DAG in the MCMC chain.
#' The MAP estimate is represented through its \eqn{(q,q)} adjacency matrix, with \eqn{(u,v)}-element equal to one whenever the MAP contains \eqn{u -> v},
#' zero otherwise.
#'
#' @author Federico Castelletti and Alessandro Mascaro
#'
#'
#' @references F. Castelletti and A. Mascaro (2021). Structural learning and estimation of joint causal effects among network-dependent variables. \emph{Statistical Methods and Applications}, Advance publication.
#' @references G. Garcia-Donato and M.A. Martinez-Beneito (2013). On sampling strategies in Bayesian variable selection problems with large model spaces. \emph{Journal of the American Statistical Association} 108 340-352.
#'
#' @param learnDAG_output object of class \code{bcdag}
#'
#' @return The \eqn{(q,q)} adjacency matrix of the maximum a posteriori DAG model
#' @export
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
#' # Produce the MAP DAG estimate
#' get_MAPdag(out_mcmc)
#'
get_MAPdag <- function(learnDAG_output) {

  if (!methods::is(learnDAG_output,"bcdag")) {
    stop("learnDAG_output must be an object of class bcdag")
  }

  type <- attributes(learnDAG_output)$type

  if (type == "compressed" | type == "compressed and collapsed") {
    dag_code <- as.vector(sapply(out$Graphs, gsub, pattern = ";", replacement = ""))
    q <- sqrt(nchar(dag_code[[1]]))
  } else {
    Graphs <- learnDAG_output$Graphs
    dag_code <- apply(Graphs, 3, bd_encode, separator = "")
    q <- dim(Graphs)[1]
  }

  uniq_dag <- unique(dag_code)
  map_dagcode <- uniq_dag[which.max(tabulate(match(dag_code, uniq_dag)))]
  map_dagcode <- bd_decode(map_dagcode, separator = "")
  map_dag <- matrix(map_dagcode, ncol = q)
  colnames(map_dag) = rownames(map_dag) = 1:ncol(map_dag)
  return(map_dag)
}
