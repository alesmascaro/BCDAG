#' bcdag object print
#'
#' @param object a \code{bcdag} object for which a summary is desired
#' @param ... additional arguments affecting the summary produced
#'
#' @return A printed message listing the inputs given to learn_DAG.
#' @rdname print.bcdag
#' @export print.bcdag
#' @export
#'
#' @examples n <- 1000
#' q <- 4
#' DAG <- matrix(c(0,1,1,0,0,0,0,0,0,0,0,0,0,1,1,0), nrow = q)
#'
#' L <- DAG
#' L[L != 0] <- runif(q, 0.2, 1)
#' diag(L) <- c(1,1,1,1)
#' D <- diag(1, q)
#' Sigma <- t(solve(L))%*%D%*%solve(L)
#'
#' a <- 6
#' g <- 1/1000
#' U <- g*diag(1,q)
#' w = 0.2
#'
#' set.seed(1)
#' X <- mvtnorm::rmvnorm(n, sigma = Sigma)
#'
#' out <- learn_DAG(1000, 0, X, a, U, w, fast = TRUE, collapse = TRUE, save.memory = FALSE)
#' print(out)
print.bcdag <- function(object, ...) {
  learnDAG_output <- object
  if (validate_bcdag(learnDAG_output) == FALSE) {
    stop("learnDAG_output must be an object of class bcdag")
  }
  type = attributes(learnDAG_output)$type
  input = attributes(learnDAG_output)$input

  cat("A ", type, " bcdag object containing", input$S, "draws from",
      ifelse(type == "collapsed" | type == "compressed and collapsed", "the posterior distribution of DAGs.", "the joint posterior over DAGs, L and D."),
      "(Burnin =", input$burn, ").",
      ifelse(type == "compressed" | type == "compressed and collapsed", "\n\nThe output is saved as strings (option save.memory = TRUE)", " "))
  cat("\n\nPrior hyperparameters: ", "\nw = ", input$w, "\na = ", input$a, "\nU =\n")
  print(input$U)
}