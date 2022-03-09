#' bcdag object summaries
#'
#' @param object a \code{bcdag} object for which a summary is desired
#' @param ... additional arguments affecting the summary produced
#'
#' @return A printed message listing the inputs given to learn_DAG, a graphical representation of the Median Probability DAG, a heatmap representing posterior probability of edge inclusion and an histogram of the number of edges in the visited DAGs.
#' @rdname summary.bcdag
#' @export summary.bcdag
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
#' summary(out)
summary.bcdag <- function(object, ...) {
  learnDAG_output <- object
  if (validate_bcdag(learnDAG_output) == FALSE) {
    stop("learnDAG_output must be an object of class bcdag")
  }

  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))

  type = attributes(learnDAG_output)$type
  input = attributes(learnDAG_output)$input

  cat("A ", type, " bcdag object containing ", input$S, " draws from",
      ifelse(type == "collapsed" | type == "compressed and collapsed", " the posterior distribution of DAGs.", "the joint posterior over DAGs, L and D."),
      "(Burnin =", input$burn, ").",
      ifelse(type == "compressed" | type == "compressed and collapsed", "\n\nThe output is saved as strings (option save.memory = TRUE)", " "))
  cat("\n\nPrior hyperparameters: ", "\nw = ", input$w, "\na = ", input$a, "\nU =")
  print(U)

  edgeprobs <- get_edgeprobs(learnDAG_output)
  MPMdag <- get_MPMdag(learnDAG_output)
  Graphsizes <- vector("double", input$S)
  for (i in 1:input$S) {
    if (type == "compressed" | type == "compressed and collapsed") {
      Graphsizes[i] <- sum(bd_decode(learnDAG_output$Graphs[i]))
    } else {
      Graphsizes[i] <- sum(learnDAG_output$Graphs[,,i])
    }
  }
  graphics::par(pty = "s")
  gRbase::plot(methods::as(MPMdag, "graphNEL"), main = "Median probability DAG")
  grDevices::devAskNewPage(ask = TRUE)
  c = grDevices::gray.colors(20, start = 1, end = 0, gamma = 1, alpha = NULL)
  print(lattice::levelplot(edgeprobs, xlab = "From", ylab = "Into", col.regions = c, main = "Probabilities of edge inclusion"))
  print(lattice::histogram(Graphsizes, probability = TRUE, col = "grey", ylab = "% on total", main = "Distribution of DAGs size"))
  grDevices::devAskNewPage(ask = FALSE)
}
