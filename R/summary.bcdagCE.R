#' bcdagCE object summary
#'
#' @param object a \code{bcdagCE} object for which a summary is desired
#' @param ... additional arguments affecting the summary produced
#'
#' @return A printed message listing the inputs given to learn_DAG and get_causaleffect() and summary statistics of the posterior distribution.
#' @rdname summary.bcdagCE
#' @export summary.bcdagCE
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
#'                      fast = TRUE, save.memory = FALSE)
#' out_ce <- get_causaleffect(out, targets = c(4,6), response = 1))
#' summary(out_ce)
summary.bcdagCE <- function(object, ...) {
  getCE_output <- object
  if (!methods::is(object,"bcdagCE")) {
    stop("learnDAG_output must be an object of class bcdagCE")
  }
  type = attributes(getCE_output)$type
  input = attributes(getCE_output)$input

  targets <- as.numeric(input[base::grep("targets", names(input))])

  cat("A ", type, " bcdagCE object containing", input$S, "draws from the posterior distribution of causal effects of variables ",
      paste(targets, collapse = ", "), "on ", input$response)
  cat("\n\nPrior hyperparameters: ", "\nw = ", input$w, "\na = ", input$a, "\nU =\n")
  print(input$U)

  cat("\nPosterior means of causal effects: \n")
  print(getCE_output$post_mean)
  cat("\nPosterior quantiles of causal effects: \n")
  print(t(getCE_output$post_ci))
  cat("\nPosterior probability of causal effects being 0: \n")
  print(getCE_output$Pr0)
  cat("\nPosterior probability of causal effects being greater than 0: \n")
  print(getCE_output$Prg0)
}
