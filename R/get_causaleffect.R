#' Estimate total causal effects from the MCMC output
#'
#' This function provides causal effect estimates from the output of \code{learn_DAG}
#'
#' Output of \code{learn_dag} function consists of \eqn{S} draws from the joint posterior of DAGs and DAG-parameters in a zero-mean Gaussian DAG-model;
#' see the documentation of \code{learn_DAG} for more details.
#'
#' The total causal effect on a given variable of interest (\code{response}) consequent to a joint intervention on a set of variables (\code{targets})
#' is defined according to Pearl's do-calculus theory and under the Gaussian assumption can be expressed as a function of parameters \code{(D,L)},
#' representing a (Cholesky) reparameterization of the covariance matrix.
#'
#' Specifically, to each intervened variable a causal effect coefficient is associated and the posterior distribution of the latter can be recovered from posterior draws
#' of the DAG parameters returned by \code{learn_DAG}. For each coefficient a sample of size \eqn{S} from its posterior is available. If required, the only
#' Bayesian Model Average (BMA) estimate (obtained as the sample mean of the \eqn{S} draws) can be returned by setting \code{BMA = TRUE}.
#'
#' Notice that, whenever implemented with \code{collapse = FALSE}, \code{learn_DAG} returns the marginal posterior distribution of DAGs only.
#' In this case, \code{get_causaleffect} preliminarly performs posterior inference of DAG parameters by drawing samples from the posterior of \code{(D,L)}.
#'
#' Print, summary and plot methods are available for this function. \code{print} returns the values of the prior hyperparameters used in the learnDAG function. \code{summary} returns, for each causal effect parameter, the marginal posterior mean and quantiles for different \eqn{\alpha} levels, and posterior probabilities of negative, null and positive causal effects. \code{plot} provides graphical summaries (boxplot and histogram of the distribution) for the posterior of each causal effect parameter.
#'
#' @author Federico Castelletti and Alessandro Mascaro
#'
#' @references J. Pearl (2000). \emph{Causality: Models, Reasoning, and Inference}. Cambridge University Press, Cambridge.
#' @references F. Castelletti and A. Mascaro (2021) Structural learning and estimation of joint causal effects among network-dependent variables. \emph{Statistical Methods and Applications}, Advance publication.
#' @references  P. Nandy, M.H. Maathuis and T. Richardson (2017). Estimating the effect of joint interventions from observational data in sparse high-dimensional settings. \emph{Annals of Statistics} 45(2), 647-674.
#'
#'
#' @param targets numerical \eqn{(p,1)} vector with labels of target nodes
#' @param response numerical label of response variable
#' @param learnDAG_output object of class \code{bcdag}
#' @param verbose if \code{TRUE}, progress bar of MCMC sampling is displayed
#'
#' @return An S3 object of class \code{bcdagCE} containing \eqn{S} draws from the joint posterior distribution of the \eqn{p} causal effect coefficients, organized into an \eqn{(S,p)} matrix, posterior means and credible intervals (under different \eqn{(1-\alpha)} levels) for each causal effect coefficient, and marginal posterior probabilities of positive, null and negative causal effects.
#'
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
#' n = 200
#' # Generate observations from a Gaussian DAG-model
#' X = mvtnorm::rmvnorm(n = n, sigma = Sigma)
#' # Run the MCMC (set S = 5000 and burn = 1000 for better results)
#' out_mcmc = learn_DAG(S = 500, burn = 100, a = q, U = diag(1,q)/n, data = X, w = w,
#'                      fast = TRUE, save.memory = FALSE)
#' head(out_mcmc$Graphs)
#' head(out_mcmc$L)
#' head(out_mcmc$D)
#' # Compute the BMA estimate of coefficients representing
#' # the causal effect on node 1 of an intervention on {3,4}
#' out_causal = get_causaleffect(learnDAG_output = out_mcmc, targets = c(3,4), response = 1)$post_mean
#'
#' # Methods
#' print(out_causal)
#' summary(out_causal)
#' plot(out_causal)

get_causaleffect <- function(learnDAG_output, targets, response, verbose = TRUE) {
    ## Input check

  learnDAGinput_check <- methods::is(learnDAG_output, "bcdag")
  targets_check <- is.numeric(targets) & is.vector(targets)
  if (targets_check) {
    targets_check <- targets_check & all(targets %% 1 == 0) &
      prod(targets > 0 & targets <= dim(learnDAG_output$Graphs)[2]) == 1
  }

  response_check <- is.numeric(response) & length(response) == 1 & (response %% 1 == 0) &
    prod((response > 0 & response <= dim(learnDAG_output)[2])) == 1
  if (response_check) {
    response_check <- response_check & prod(response %% 1 == 0) &
      prod(response > 0 & response <= dim(learnDAG_output$Graphs)[2]) == 1
  }

  if(learnDAGinput_check == FALSE) stop("learnDAG_output must be an object of class bcdag")

  type <- attributes(learnDAG_output)$type
  input <- attributes(learnDAG_output)$input
  S <- input$S
  n <- nrow(input$data)
  q <- ncol(input$data)
  X <- scale(input$data, scale = FALSE)
  tXX = crossprod(X)

  if(targets_check == FALSE) stop("targets must be a vector containing the position of intervention targets in the dataset")
  if(response_check == FALSE) stop("response must be the numerical value indicating the position of the response variable in the dataset")

  collapsed <- type == "compressed and collapsed" | type == "collapsed"

  if (type == "compressed" | type == "compressed and collapsed") {  # If option save.memory == TRUE
    Graphs <- array(dim = c(q,q,S))
    L <- array(dim = c(q,q,S))
    D <- array(dim = c(q,q,S))
    if (collapsed == FALSE) {
      for (i in 1:S) {
        Graphs[,,i] <- bd_decode(learnDAG_output$Graphs[i])
        L[,,i] <- bd_decode(learnDAG_output$L[i])
        D[,,i] <- bd_decode(learnDAG_output$D[i])
      }
    } else {
      if (verbose == TRUE) {
        cat("\nSampling parameters...")
        pb <- utils::txtProgressBar(min = 2, max = S, style = 3)
      }
      for (i in 1:S) {
        Graphs[,,i] <- bd_decode(learnDAG_output$Graphs[i])
        postparams <- rDAGWishart(1, Graphs[,,i], input$a+n, input$U+tXX)
        L[,,i] <- postparams$L
        D[,,i] <- postparams$D
        if (verbose == TRUE) {
          utils::setTxtProgressBar(pb, i)
          close(pb)
        }
      }
    }
  } else {                                              # If option save.memory == FALSE
    Graphs <- learnDAG_output$Graphs
    if (collapsed == FALSE) {
      L <- learnDAG_output$L
      D <- learnDAG_output$D
    } else {
      L <- array(0, c(q,q,S))
      D <- array(0, c(q,q,S))
      if (verbose == TRUE) {
        cat("\nSampling parameters...")
        pb <- utils::txtProgressBar(min = 2, max = S, style = 3)
      }
      for (i in 1:S) {
        postparams <- rDAGWishart(1, Graphs[,,i], input$a+n, input$U+tXX)
        L[,,i] <- postparams$L
        D[,,i] <- postparams$D
        if (verbose == TRUE) {
          utils::setTxtProgressBar(pb, i)
          close(pb)
        }
      }
    }
  }

  causaleffects <- matrix(0, ncol = length(targets), nrow = S)
  colnames(causaleffects) = paste0("h = ", targets)

  for (i in 1:S) {
    causaleffects[i,] <- causaleffect(targets, response, L[,,i], D[,,i])
  }

  postmean <- base::apply(causaleffects, 2, mean)
  postquantiles <- base::apply(causaleffects, 2, stats::quantile, c(0.025, 0.25, 0.5, 0.75, 0.975))
  Probs <- matrix(0, ncol = 3, nrow = length(targets))
  Probs[,1] <- base::colMeans(causaleffects < 0)
  Probs[,2] <- base::colMeans(causaleffects == 0)
  Probs[,3] <- base::colMeans(causaleffects > 0)
  rownames(Probs) <- paste0("h = ", targets)
  colnames(Probs) <- c("<0", "=0", ">0")

  out_ce <- list(causaleffects = causaleffects, post_mean = postmean,
                 post_ci = postquantiles, Probs = Probs)

  input <- c(input, targets = targets, response = response)

  out <- new_bcdagCE(out_ce, input = input, type = type)
}
