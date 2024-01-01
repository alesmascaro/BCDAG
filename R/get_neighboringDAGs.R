#' Enumerate all neighbors of a DAG
#'
#' This functions takes any DAG with \eqn{q} nodes as input and returns all the neighboring DAGs, i.e. all those DAGs that
#' can be reached by the addition, removal or reversal of an edge.
#'
#' @param DAG Adjacency matrix of a DAG
#'
#' @return The \eqn{(q,q,K)} array containing all neighboring DAGs, with \eqn{K} being the total number of neighbors
#' @export
#'
#' @examples # Randomly generate a DAG
#' q <- 4; w <- 0.2
#' set.seed(123)
#' DAG <- rDAG(q,w)
#' # Get neighbors
#' neighbors <- get_neighboringDAGs(DAG)
#' neighbors

get_neighboringDAGs <- function(DAG) {
  if (gRbase::is.DAG(DAG) == FALSE) {
    stop("Input must be a Directed Acyclic Graph")
  }

  A <- DAG
  q <- ncol(A)
  A_na <- A
  diag(A_na) <- NA

  # Define the set of possible operations!
  id_set = c()
  dd_set = c()
  rd_set = c()

  ## set of nodes for id
  set_id = which(A_na == 0, TRUE)
  if(length(set_id) != 0){
    id_set = cbind(1, set_id)
  }

  ## set of nodes for dd
  set_dd = which(A_na == 1, TRUE)
  if(length(set_dd != 0)){
    dd_set = cbind(2, set_dd)
  }

  ## set of nodes for rd
  set_rd = which(A_na == 1, TRUE)
  if(length(set_rd != 0)){
    rd_set = cbind(3, set_rd)
  }

  O = rbind(id_set, dd_set, rd_set)
  neighbors <- array(0,c(q,q,nrow(O)))
  isDAGvec <- vector(length = nrow(O))
  for (i in 1:nrow(O)) {
    neighbors[,,i] <- operation(O[i,1], DAG, O[i,2:3])
    isDAGvec[i] <- gRbase::is.DAG(neighbors[,,i])
  }
  neighbors <- neighbors[,,isDAGvec]
  return(neighbors)
}
