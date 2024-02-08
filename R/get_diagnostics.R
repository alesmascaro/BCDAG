#' MCMC diagnostics
#'
#' This function provides diagnostics of convergence for the MCMC output of \code{learn_DAG} function.
#'
#' Function \code{learn_DAG} implements a Markov Chain Monte Carlo (MCMC) algorithm for structure learning and posterior inference of Gaussian DAGs.
#' Output of the algorithm is a collection of \eqn{S} DAG structures (represented as \eqn{(q,q)} adjacency matrices) and DAG parameters \eqn{(D,L)}
#' approximately drawn from the joint posterior.
#' In addition, if \code{learn_DAG} is implemented with \code{collapse = TRUE}, the only approximate marginal posterior of DAGs (represented by the collection of \eqn{S} DAG structures) is returned;
#' see the documentation of \code{learn_DAG} for more details.
#'
#' Diagnostics of convergence for the MCMC output are conducted by monitoring across MCMC iterations: (1) the number of edges in the DAGs;
#' (2) the posterior probability of edge inclusion for each possible edge \eqn{u -> v}.
#' With regard to (1), a traceplot of the number of edges in the DAGs visited by the MCMC chain at each step \eqn{s = 1, ..., S} is first provided as the output of the function.
#' The absence of trends in the plot can provide information on a genuine convergence of the MCMC chain.
#' In addition, the traceplot of the average number of edges in the DAGs visited up to time \eqn{s}, for \eqn{s = 1, ..., S}, is also returned.
#' The convergence of the curve around a "stable" average size generally suggests good convergence of the algorithm.
#' With regard to (2), for each edge \eqn{u -> v}, the posterior probability at time \eqn{s}, for \eqn{s = 1, ..., S}, can be estimated as
#' as the proportion of DAGs visited by the MCMC up to time \eqn{s} which contain the directed edge \eqn{u -> v}.
#' Output is organized in \eqn{q} plots (one for each node \eqn{v = 1, ..., q}), each summarizing the posterior probabilities of edges \eqn{u -> v}, \eqn{u = 1, ..., q}.
#' If the number of nodes is larger than 30 the traceplot of a random sample of 30 nodes is returned.
#'
#' @param learnDAG_output object of class \code{bcdag}
#' @param ask Boolean argument passed to par() for visualization;
#' @param nodes Numerical vector indicating those nodes for which we want to compute the posterior probability of edge inclusion;
#'
#' @return A collection of plots summarizing the behavior of the number of edges and the posterior probabilities of edge inclusion computed from the MCMC output.
#' @export
#'
#' @author Federico Castelletti and Alessandro Mascaro
#'
#' @references F. Castelletti and A. Mascaro (2021). Structural learning and estimation of joint causal effects among network-dependent variables. \emph{Statistical Methods and Applications}, Advance publication.
#' @references F. Castelletti (2020). Bayesian model selection of Gaussian Directed Acyclic Graph structures. \emph{International Statistical Review} 88 752-775.
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
#' # Run the MCMC for posterior inference of DAGs only (collapse = TRUE)
#' out_mcmc = learn_DAG(S = 5000, burn = 1000, a = q, U = diag(1,q)/n, data = X, w = 0.1,
#'                                    fast = TRUE, save.memory = FALSE, collapse = TRUE)
#' # Produce diagnostic plots
#' get_diagnostics(out_mcmc)

get_diagnostics <- function(learnDAG_output, ask = TRUE, nodes = integer(0)) {
  if (!methods::is(learnDAG_output,"bcdag")) {
    stop("learnDAG_output must be an object of class bcdag")
  }

  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))

  type <- attributes(learnDAG_output)$type
  input <- attributes(learnDAG_output)$input

  if (type == "compressed" | type == "compressed and collapsed") {
    S <- length(learnDAG_output$Graphs)
    q <- ncol(bd_decode(learnDAG_output$Graphs[1]))
    Graphs <- array(dim = c(q,q,S))
    for (i in 1:S) {
      Graphs[,,i] <- bd_decode(learnDAG_output$Graphs[i])
    }
  } else {
    S <- input$S
    q <- ncol(input$data)
    Graphs <- learnDAG_output$Graphs
  }

    ## Graph size
  Graphsizes <- vector("double", S)
  for (i in 1:S) {
    Graphsizes[i] <- sum(Graphs[,,i])
  }

    ## Cumulative edge probabilities
  cumedgesum <- Graphs
  cumedgeprob <- Graphs
  for (i in 2:S) {
    cumedgesum[,,i] <- (cumedgesum[,,i] + cumedgesum[,,i-1])
    cumedgeprob[,,i] <- cumedgesum[,,i]/i
  }
  tracematrices <- vector("list", q)
  whc_iter <- lapply(1:q, function(i) as.matrix(1:S))
  whcs <- vector("list", q)
  for (j in 1:q) {
    whc <- which(cumedgeprob[,j,S] >= 0.05)
    whcs[[j]] <- whc[order(cumedgeprob[whc, j, S], decreasing = TRUE)]
    if (length(whcs[[j]]) != 1) {
      tracematrices[[j]] <- t(as.matrix(cumedgeprob[whcs[[j]],j,]))
    } else {
      tracematrices[[j]] <- as.matrix(cumedgeprob[whcs[[j]],j,])
    }

    if (S > 10000) {
      whc_iter[[j]] <- seq(1,S, length.out = 10000)
      if (length(whc) > 1) {
        tracematrices[[j]] <- tracematrices[[j]][whc_iter[[j]],]
      } else {
        tracematrices[[j]] <- as.matrix(tracematrices[[j]][whc_iter[[j]]])
      }
    }
  }
    ## Plotting
  graphics::par(mfrow = c(1,2), ask = ask)
  base::plot(1:S, Graphsizes, type = "l", xlab = "Iteration", ylab = "graph size", main = "Graph size traceplot", col = 1)
  base::plot(1:S, cumsum(Graphsizes)/(1:S), type = "l", xlab = "Iteration", ylab = "average graph size", main = "Graph size running mean", col = 2)
  if (length(nodes) == 0) {
    graphics::par(ask = ask)
    if(q <= 30) {
      graphics::par(mfrow = c(2,3))
      for (j in 1:q) {
        graphics::matplot(y = tracematrices[[j]], x = whc_iter[[j]], type = "l", xlab = "Iteration", ylab = "prob. of inclusion", main = paste("Into node", j), ylim = c(0,1))
        if (length(whcs[[j]]) != 0) graphics::legend("topleft", legend = utils::head(whcs[[j]], 6), col = 1:max(length(whcs[[j]]), 6), lty = 1, cex = 0.75)
      }
    } else {
      graphics::par(mfrow = c(2,3))
      randomnodes <- sample(1:q, 30)
      for (j in randomnodes) {
        graphics::matplot(y = tracematrices[[j]], x = whc_iter[[j]], type = "l", xlab = "Iteration", ylab = "prob. of inclusion", main = paste("Into node", j), ylim = c(0,1))
        if (length(whcs[[j]]) != 0) graphics::legend("topleft", legend = utils::head(whcs[[j]], 6), col = 1:max(length(whcs[[j]]), 6), lty = 1, cex = 0.75)
      }
    }
  } else {
    if(length(nodes) <= 30) {
      for (j in nodes) {
        graphics::par(ask = ask, mfrow = c(1,1))
        graphics::matplot(y = tracematrices[[j]], x = whc_iter[[j]], type = "l", xlab = "Iteration", ylab = "prob. of inclusion", main = paste("Into node", j), ylim = c(0,1))
        if (length(whcs[[j]]) != 0) graphics::legend("topleft", legend = utils::head(whcs[[j]], 6), col = 1:max(length(whcs[[j]]), 6), lty = 1, cex = 0.75)
      }
    } else {
      graphics::par(mfrow = c(2,3), ask = ask)
      randomnodes <- sample(nodes, 30)
      for (j in randomnodes) {
        graphics::matplot(y = tracematrices[[j]], x = whc_iter[[j]], type = "l", xlab = "Iteration", ylab = "prob. of inclusion", main = paste("Into node", j), ylim = c(0,1))
        if (length(whcs[[j]]) != 0) graphics::legend("topleft", legend = utils::head(whcs[[j]], 6), col = 1:max(length(whcs[[j]]), 6), lty = 1, cex = 0.75)
      }
    }
  }
}

