#' Compute causal effects between variables
#'
#' This function computes the total joint causal effect on variable \code{response} consequent to an intervention on variables \code{targets}
#' for a given a DAG structure and parameters \code{(D,L)}
#'
#' We assume that the joint distribution of random variables \eqn{X_1, \dots, X_q} is zero-mean Gaussian with covariance matrix Markov w.r.t. a Directed Acyclic Graph (DAG).
#' In addition, the allied Structural Equation Model (SEM) representation of a Gaussian DAG-model allows to express the covariance matrix as a function of the (Cholesky) parameters \code{(D,L)},
#' collecting the conditional variances and regression coefficients of the SEM.
#'
#' The total causal effect on a given variable of interest (\code{response}) consequent to a joint intervention on a set of variables (\code{targets})
#' is defined according to Pearl's do-calculus theory and under the Gaussian assumption can be expressed as a function of parameters \code{(D,L)}.
#'
#' @author Federico Castelletti and Alessandro Mascaro
#'
#' @references J. Pearl (2000). \emph{Causality: Models, Reasoning, and Inference}. Cambridge University Press, Cambridge.
#' @references F. Castelletti and A. Mascaro (2021). Structural learning and estimation of joint causal effects among network-dependent variables. \emph{Statistical Methods and Applications}, Advance publication.
#' @references  P. Nandy, M.H. Maathuis and T. Richardson (2017). Estimating the effect of joint interventions from observational data in sparse high-dimensional settings. \emph{Annals of Statistics} 45(2), 647-674.
#'
#' @param targets numerical vector with labels of target nodes
#' @param response numerical label of response variable
#' @param L \eqn{(q,q)} matrix of regression-coefficient parameters
#' @param D \eqn{(q,q)} diagonal matrix of conditional-variance parameters
#'
#' @return The joint total causal effect, represented as a vector of same length of \code{targets}
#' @export
#'
#' @examples # Randomly generate a DAG and the DAG-parameters
#' q = 8
#' w = 0.2
#' set.seed(123)
#' DAG = rDAG(q = q, w = w)
#' outDL = rDAGWishart(n = 1, DAG = DAG, a = q, U = diag(1, q))
#' L = outDL$L; D = outDL$D
#' # Total causal effect on node 1 of an intervention on {5,6}
#' causaleffect(targets = c(6,7), response = 1, L = L, D = D)
#' # Total causal effect on node 1 of an intervention on {5,7}
#' causaleffect(targets = c(5,7), response = 1, L = L, D = D)
#'
causaleffect <- function(targets, response, L, D){

  targets_check <- is.numeric(targets) & is.vector(targets)
  if (targets_check) {
    targets_check <- targets_check & prod(targets %% 1 == 0) & prod(targets > 0 & targets <= ncol(L)) == 1
  }
  response_check <- is.numeric(response) & length(response) == 1
  if (response_check) {
    response_check <- response_check & (response %% 1 == 0) &
      prod((response > 0 & response <= ncol(L))) == 1
  }
  L_check <- is.numeric(L) & is.matrix(L) & dim(L)[1] == dim(L)[2]
  L_dagcheck <- gRbase::is.DAG((L - diag(diag(L)) != 0)*1)
  D_check <- is.numeric(L) & is.matrix(L) & dim(L)[1] == dim(L)[2] &
    all(D[lower.tri(D)] == 0, D[upper.tri(D)] == 0) & all(D >= 0)

  if(targets_check == FALSE) stop("targets must be a vector containing the position of intervention targets in the dataset")
  if(response_check == FALSE) stop("response must be the numerical value indicating the position of the response variable in the dataset")
  if(L_check == FALSE) stop("L must be a qxq matrix of regression coefficient parameters")
  if(L_dagcheck == FALSE) stop("L is not a matrix of coefficients of an acyclic SEM")
  if(D_check == FALSE) stop("D must be a qxq diagonal matrix of conditional variance parameters")

  if(length(unique(targets)) != length(targets)) warning("Your vector of targets does not contain distinct elements")

  y <- response

  L_I <- L
  L_I[,targets] = 0
  diag(L_I) <- 1
  Sigma_I <- solve(t(L_I)) %*% D %*% solve(L_I)

  effects <- sapply(targets, function(x) Sigma_I[x,y]/Sigma_I[x,x])

  return(effects)
}
