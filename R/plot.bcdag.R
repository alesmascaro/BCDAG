#' bcdag object plot
#'
#' This method returns summary plots of the output of \code{learn_DAG()}.
#'
#' @param x a \code{bcdag} object for which a plot is desired
#' @param ask Boolean argument passed to par() for visualization;
#' @param ... additional arguments affecting the summary produced
#'
#' @return Plot of the Median Probability DAG, a heatmap of the probabilities of edge inclusion and an histogram of the sizes of graphs visited by learn_DAG().
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
#' plot(out)
plot.bcdag <- function(x, ..., ask = TRUE) {
  learnDAG_output <- x
  if (!methods::is(x,"bcdag")) {
    stop("learnDAG_output must be an object of class bcdag")
  }

  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))

  type = attributes(learnDAG_output)$type
  input = attributes(learnDAG_output)$input

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
  Rgraphviz::plot(as_graphNEL(MPMdag), main = "Median probability DAG")
  grDevices::devAskNewPage(ask = ask)
  c = grDevices::gray.colors(20, start = 1, end = 0, gamma = 1, alpha = NULL)
  print(lattice::levelplot(t(edgeprobs), xlab = "Into", ylab = "From", col.regions = c, main = "Probabilities of edge inclusion"))
  print(lattice::histogram(Graphsizes, probability = TRUE, col = "grey", ylab = "% on total", main = "Distribution of DAGs size"))
  grDevices::devAskNewPage(ask = FALSE)
}
