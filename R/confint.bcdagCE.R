#' Confidence Intervals for bcdagCE Object
#'
#' Computes credible (not confidence!) intervals for one or more target variables from objects of class \code{bcdagCE}.
#'
#' @param object a \code{bcdagCE} object from which credible intervals are computed.
#' @param parm a vector specifying which target variables are to be given confidence intervals. If missing, all variables are considered.
#' @param level the credible level required.
#' @param ... additional arguments.
#'
#' @return A matrix with columns giving lower and upper credible limits for each variable..
#' @export
#'
#' @examples q = 8
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
#'                      fast = TRUE, save.memory = FALSE, verbose = FALSE)
#' out_ce <- get_causaleffect(out_mcmc, targets = c(4,6), response = 1)
#' # confint(out_ce, c(4,6), 0.95)

confint.bcdagCE <- function(object, parm = "all", level = 0.95, ...) {
  if (!methods::is(object,"bcdagCE")) {
    stop("learnDAG_output must be an object of class bcdagCE")
  }
  if (!(level > 0 & level < 1)) {
    stop("level must be in (0,1)")
  }
  if (prod(parm == "all") == 1) {
    alpha <- 1-level
    ci <- base::apply(as.matrix(object$causaleffect), 2, stats::quantile, c(alpha/2, 1-alpha/2))
    ci <- t(ci)
  } else {
    coln <- colnames(object$causaleffects)
    coln <- sapply(coln, function(j) as.integer(substr(j, nchar(j), nchar(j))))
    ind <- parm %in% coln
    if (sum(!ind) != 0) {
      stop("bcdagCE object does not include all the selected variables")
    }
    whc <- which(ind)
    alpha <- 1-level
    ci <- base::apply(as.matrix(object$causaleffect[,whc]), 2, stats::quantile, c(alpha/2, 1-alpha/2))
    ci <- t(ci); rownames(ci) <- paste0("h = ", parm[whc])
  }
  return(ci)
}
