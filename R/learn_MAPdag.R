#' Maximum a posteriori DAG estimation from mcmc output
#'
#' @param learnDAG_output output of learn_dag()
#'
#' @return Adjacency matrix of MAP DAG
#' @export
#'
#' @examples
learn_MAPdag <- function(learnDAG_output) {

  if (class(learnDAG_output$Graphs) == "character") {
    dag_code <- sapply(out$Graphs, gsub, pattern = ";", replacement = "")
    q <- sqrt(nchar(dag_code[[1]]))
    # dag_code <- as.numeric(dag_code)
  } else {
    Graphs <- learnDAG_output$Graphs
    dag_code <- apply(Graphs, 3, bd_encode, separator = "")
    dag_code <- as.numeric(dag_code)
    q <- dim(Graphs)[1]
  }

  uniq_dag <- unique(dag_code)
  map_dagcode <- uniq_dag[which.max(tabulate(match(dag_code, uniq_dag)))]
  map_dag <- matrix(map_dagcode, ncol = q)

  return(map_dag)
}
