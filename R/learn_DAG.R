#' MCMC scheme for Gaussian DAG posterior inference
#'
#' This function implements a Markov Chain Monte Carlo (MCMC) algorithm for structure learning of Gaussian
#' DAGs and posterior inference of DAG model parameters
#'
#' Consider a collection of random variables \eqn{X_1, \dots, X_q} whose distribution is zero-mean multivariate Gaussian with covariance matrix Markov w.r.t. a Directed Acyclic Graph (DAG).
#' Assuming the underlying DAG is unknown (model uncertainty), a Bayesian method for posterior inference on the joint space of DAG structures and parameters can be implemented.
#' The proposed method assigns a prior on each DAG structure through independent Bernoulli distributions, \eqn{Ber(w)}, on the 0-1 elements of the DAG adjacency matrix.
#' Conditionally on a given DAG, a prior on DAG parameters \eqn{(D,L)} (representing a Cholesky-type reparameterization of the covariance matrix) is assigned through a compatible DAG-Wishart prior;
#' see also function \code{rDAGWishart} for more details.
#'
#' Posterior inference on the joint space of DAGs and DAG parameters is carried out through a Partial Analytic Structure (PAS) algorithm.
#' Two steps are iteratively performed for \eqn{s = 1, 2, ...} : (1) update of the DAG through a Metropolis Hastings (MH) scheme;
#' (2) sampling from the posterior distribution of the (updated DAG) parameters.
#' In step (1) the update of the (current) DAG is performed by drawing a new (direct successor) DAG from a suitable proposal distribution. The proposed DAG is obtained by applying a local move (insertion, deletion or edge reversal)
#' to the current DAG and is accepted with probability given by the MH acceptance rate.
#' The latter requires to evaluate the proposal distribution at both the current and proposed DAGs, which in turn involves the enumeration of
#' all DAGs that can be obtained from local moves from respectively the current and proposed DAG.
#' Because the ratio of the two proposals is approximately equal to one, and the approximation becomes as precise as \eqn{q} grows, a faster strategy implementing such an approximation is provided with
#' \code{fast = TRUE}. The latter choice is especially recommended for moderate-to-large number of nodes \eqn{q}.
#'
#' Output of the algorithm is a collection of \eqn{S} DAG structures (represented as \eqn{(q,q)} adjacency matrices) and DAG parameters \eqn{(D,L)} approximately drawn from the joint posterior.
#' The various outputs are organized in \eqn{(q,q,S)} arrays; see also the example below.
#' If the target is DAG learning only, a collapsed sampler implementing the only step (1) of the MCMC scheme can be obtained
#' by setting \code{collapse = TRUE}. In this case, the algorithm outputs a collection of \eqn{S} DAG structures only.
#' See also functions \code{get_edgeprobs}, \code{get_MAPdag}, \code{get_MPMdag} for posterior summaries of the MCMC output.
#'
#' @author Federico Castelletti and Alessandro Mascaro
#'
#' @references F. Castelletti and A. Mascaro (2021). Structural learning and estimation of joint causal effects among network-dependent variables. \emph{Statistical Methods and Applications}, Advance publication.
#' @references F. Castelletti and A. Mascaro (2022). BCDAG: An R package for Bayesian structural and Causal learning of Gaussian DAGs. \emph{arXiv pre-print}, url: https://arxiv.org/abs/2201.12003
#' @references F. Castelletti (2020). Bayesian model selection of Gaussian Directed Acyclic Graph structures. \emph{International Statistical Review} 88 752-775.
#'
#' @param S integer final number of MCMC draws from the posterior of DAGs and parameters
#' @param burn integer initial number of burn-in iterations, needed by the MCMC chain to reach its stationary distribution and not included in the final output
#' @param data \eqn{(n,q)} data matrix
#' @param a common shape hyperparameter of the compatible DAG-Wishart prior, \eqn{a > q - 1}
#' @param U position hyperparameter of the compatible DAG-Wishart prior, a \eqn{(q, q)} s.p.d. matrix
#' @param w edge inclusion probability hyperparameter of the DAG prior in \eqn{[0,1]}
#' @param fast boolean, if \code{TRUE} an approximate proposal for the MCMC moves is implemented
#' @param save.memory boolean, if \code{TRUE} MCMC draws are stored as strings, instead of arrays
#' @param collapse boolean, if \code{TRUE} only structure learning of DAGs is performed
#' @param verbose If \code{TRUE}, progress bars are displayed
#'
#' @return An S3 object of class \code{bcdag} containing \eqn{S} draws from the posterior of DAGs and (if \code{collapse = FALSE}) of DAG parameters \eqn{D} and \eqn{L}. If \code{save.memory = FALSE}, these are stored in three arrays of dimension \eqn{(q,q,S)}. Otherwise, they are stored as strings.
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
#'
#' ## Set S = 5000 and burn = 1000 for better results
#'
#' # [1] Run the MCMC for posterior inference of DAGs and parameters (collapse = FALSE)
#' out_mcmc = learn_DAG(S = 50, burn = 10, a = q, U = diag(1,q)/n, data = X, w = 0.1,
#'                      fast = FALSE, save.memory = FALSE, collapse = FALSE)
#' # [2] Run the MCMC for posterior inference of DAGs only (collapse = TRUE)
#' out_mcmc_collapse = learn_DAG(S = 50, burn = 10, a = q, U = diag(1,q)/n, data = X, w = 0.1,
#'                               fast = FALSE, save.memory = FALSE, collapse = TRUE)
#' # [3] Run the MCMC for posterior inference of DAGs only with approximate proposal
#' # distribution (fast = TRUE)
#' # out_mcmc_collapse_fast = learn_DAG(S = 50, burn = 10, a = q, U = diag(1,q)/n, data = X, w = 0.1,
#' #                                    fast = FALSE, save.memory = FALSE, collapse = TRUE)
#' # Compute posterior probabilities of edge inclusion and Median Probability DAG Model
#' # from the MCMC outputs [2] and [3]
#' get_edgeprobs(out_mcmc_collapse)
#' # get_edgeprobs(out_mcmc_collapse_fast)
#' get_MPMdag(out_mcmc_collapse)
#' # get_MPMdag(out_mcmc_collapse_fast)

learn_DAG <- function(S, burn,
                      data, a, U, w,
                      fast = FALSE, save.memory = FALSE, collapse = FALSE,
                      verbose = TRUE) {

  input <- as.list(environment())

  ## Input check

  data_check <- sum(is.na(data)) == 0
  S.burn_check <- is.numeric(c(S,burn)) & length(S) == 1 & length(burn) == 1
  S.burn_check <- if (S.burn_check) {
    S.burn_check & (S %% 1 == 0) & (burn %% 1 == 0) # verify if is.integer() can be used
  } else {
    S.burn_check
  }
  a_check <- is.numeric(a) & (length(a) == 1) & (a > ncol(data) - 1)
  w_check <- is.numeric(w) & (length(w) == 1) & (w <= 1) & (w >= 0)
  U_check <- is.numeric(U) & (dim(U)[1] == dim(U)[2]) & (prod(eigen(U)$values) > 0) & isSymmetric(U)
  U.data_check <- dim(U)[1] == ncol(data)

  if (data_check == FALSE) {
    stop("Data must not contain NAs")
  }
  if (S.burn_check == FALSE) {
    stop("S and burn must be integer numbers")
  }
  if (a_check == FALSE) {
    stop("a must be at least equal to the number of variables")
  }
  if (w_check == FALSE) {
    stop("w must be a number between 0 and 1")
  }
  if (U_check == FALSE) {
    stop("U must be a squared symmetric positive definite matrix")
  }
  if (U.data_check == FALSE) {
    stop("U must be a squared spd matrix with dimensions equal to the number of variables")
  }

  n.iter <- input$burn + input$S
  X <- scale(data, scale = FALSE)
  tXX <- t(X)%*%X

  n <- dim(data)[1]
  q <- dim(data)[2]

  ## Initialize arrays or vectors depending on save.memory
  if (save.memory == TRUE) {
    Graphs <- vector("double", n.iter)
    L <- vector("double", n.iter)
    D <- vector("double", n.iter)
  } else {
    Graphs <- array(0, dim = c(q,q,n.iter))
    L <- array(0, dim = c(q,q,n.iter))
    D <- array(0, dim = c(q,q,n.iter))
  }

  currentDAG <- matrix(0, ncol = q, nrow = q)

  ## iterations

  if (save.memory == FALSE) {
    type = "collapsed"
    if (verbose == TRUE) {
      cat("Sampling DAGs...")
      pb <- utils::txtProgressBar(min = 2, max = n.iter, style = 3)
    }
    for (i in 1:n.iter) {
      prop <- propose_DAG(currentDAG, fast)
      is.accepted <- acceptreject_DAG(tXX, n,currentDAG, prop$proposedDAG,
                                      prop$op.node, prop$op.type, a, U, w,
                                      prop$current.opcard,
                                      prop$proposed.opcard)

      if (is.accepted == TRUE) {
        currentDAG <- prop$proposedDAG
      }

      Graphs[,,i] <- currentDAG
      if (verbose == TRUE) {
        utils::setTxtProgressBar(pb, i)
        close(pb)
      }
    }
    if (collapse == FALSE) {
      type = "complete"
      if (verbose == TRUE) {
        cat("\nSampling parameters...")
        pb <- utils::txtProgressBar(min = 2, max = n.iter, style = 3)
      }
      for (i in 1:n.iter) {
        postparams <- rDAGWishart(1, Graphs[,,i], a+n, U+tXX)
        L[,,i] <- postparams$L
        D[,,i] <- postparams$D
        if (verbose == TRUE) {
          utils::setTxtProgressBar(pb, i)
          close(pb)
        }
      }
    }
    Graphs <- Graphs[,,(burn+1):n.iter]
    L <- L[,,(burn+1):n.iter]
    D <- D[,,(burn+1):n.iter]
  } else {
    type = "compressed and collapsed"
    if (verbose == TRUE) {
      cat("Sampling DAGs...")
      pb <- utils::txtProgressBar(min = 2, max = n.iter, style = 3)
    }
    for (i in 1:n.iter) {
      prop <- propose_DAG(currentDAG, fast)
      is.accepted <- acceptreject_DAG(tXX, n,currentDAG, prop$proposedDAG,
                                      prop$op.node, prop$op.type, a, U, w,
                                      prop$current.opcard,
                                      prop$proposed.opcard)

      if (is.accepted == TRUE) {
        currentDAG <- prop$proposedDAG
      }

      Graphs[i] <- bd_encode(currentDAG)
      if (verbose == TRUE) {
        utils::setTxtProgressBar(pb, i)
        close(pb)
      }
    }
    if (collapse == FALSE) {
      type = "compressed"
      if (verbose == TRUE) {
        cat("\nSampling parameters...")
        pb <- utils::txtProgressBar(min = 2, max = n.iter, style = 3)
      }
      for (i in 1:n.iter) {
        postparams <- rDAGWishart(1, bd_decode(Graphs[i]), a+n, U+tXX)
        L[i] <- bd_encode(postparams$L)
        D[i] <- bd_encode(postparams$D)
        if (verbose == TRUE) {
          utils::setTxtProgressBar(pb, i)
          close(pb)
        }
      }
    }
    Graphs <- utils::tail(Graphs, S)
    L <- utils::tail(L, S)
    D <- utils::tail(D, S)
  }

  if (collapse == FALSE) {
    out <- new_bcdag(list(Graphs = Graphs, L = L, D = D), input = input, type = type)
  } else {
    out <- new_bcdag(list(Graphs = Graphs), input = input, type = type)
  }
  return(out)
}
