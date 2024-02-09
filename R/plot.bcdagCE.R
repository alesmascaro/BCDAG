#' bcdagCE object plot
#'
#' This method returns summary plots of the output of \code{get_causaleffect()}.
#'
#' @param x a \code{bcdagCE} object for which a plot is desired
#' @param ... additional arguments affecting the summary produced
#' @param which_ce specifies the list of nodes for which you intend to generate a boxplot and a histogram
#'
#' @return Boxplot and histogram of the posterior distribution of the causal effects computed using get_causaleffect().
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
#' plot(out_ce)
plot.bcdagCE <- function(x, ..., which_ce = integer(0)) {
  getCE_output <- x
  if (!methods::is(getCE_output, "bcdagCE")) {
    stop("learnDAG_output must be an object of class bcdagCE")
  }

  type = attributes(getCE_output)$type
  input = attributes(getCE_output)$input

  targets <- as.numeric(input[base::grep("targets", names(input))])
  ntargets <- length(targets)

  if (length(which_ce) == 0) {
    for (j in 1:ntargets) {
      bw <- lattice::bwplot(getCE_output$causaleffects[,j], xlab = paste0("Causal effect of ", targets[j]))
      hg <- lattice::histogram(getCE_output$causaleffects[,j], xlab = paste0("Causal effect of ", targets[j]), ylab = "Frequency")
      print(bw, split = c(1,1,2,1), more = T)
      print(hg, split = c(2,1,2,1), more = F)
    }
  } else {
    for (j in which_ce) {
      bw <- lattice::bwplot(getCE_output$causaleffects[,j], xlab = paste0("Causal effect of ", targets[j]))
      hg <- lattice::histogram(getCE_output$causaleffects[,j], xlab = paste0("Causal effect of ", targets[j]), ylab = "Frequency")
      print(bw, split = c(1,1,2,1), more = T)
      print(hg, split = c(2,1,2,1), more = F)
    }
  }
}
