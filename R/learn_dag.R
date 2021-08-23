
#' Learn an unknown DAG from data using a PAS algorithm
#'
#' @param G Desired sample size
#' @param burnin Burnin
#' @param thin thinning
#' @param data the data
#' @param a hyperparameter
#' @param U hyperparameter
#' @param w hyperparameter
#'
#' @return
#' @export
#'
#' @examples
learn_DAG <- function(G, burnin,
                data, a, U, w,
                fast = FALSE, save.memory = FALSE) {
  n.iter <- burnin + G
  X <- data
  tXX <- t(X)%*%X

  n <- dim(data)[1]
  q <- dim(data)[2]

  ## Initialize arrays or vectors
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

  pb <- txtProgressBar(min = 2, max = n.iter, style = 3)

  ## iterations

  for (i in 1:n.iter) {
    prop <- propose_DAG(currentDAG, fast)
    is.accepted <- acceptreject_DAG(X, tXX, currentDAG, prop$proposedDAG,
                                    prop$op.node, prop$op.type, a, U, w,
                                    prop$current.opcard,
                                    prop$proposed.opcard)
                                    #prop$prop.opcard)

    # occhio, l'oggetto si chiama "proposed.opcard", quindi prop$prop.opcard = NULL

    if (is.accepted == TRUE) {
      currentDAG <- prop$proposedDAG
    }

    # postparams <- rDAGWishart(1, currentDAG, a+n/2, U+tXX)
    postparams <- rDAGWishart(1, currentDAG, a+n, U+tXX)

    if (save.memory == TRUE) {
      Graphs[i] <- bd_encode(currentDAG)
      L[i] <- bd_encode(postparams$L)
      D[i] <- bd_encode(postparams$D)
    } else {
      Graphs[,,i] <- currentDAG
      L[,,i] <- postparams$L
      D[,,i] <- postparams$D
    }

    setTxtProgressBar(pb, i)
    close(pb)
  }

  if (save.memory == TRUE) {
    Graphs <- tail(Graphs, G)
    L <- tail(L, G)
    D <- tail(D, G)
  } else {
    Graphs <- Graphs[,,(burnin+1):n.iter]
    L <- L[,,(burnin+1):n.iter]
    D <- D[,,(burnin+1):n.iter]
  }

  return(list(Graphs = Graphs, L = L, D = D))

}
