#' Learn the parents of a node in a DAG
#'
#' @param q
#' @param w
#'
#' @return DAG
#' @export
#'
#' @examples
rDAG = function(q, w){
  DAG = matrix(0, q, q); colnames(DAG) = rownames(DAG) = 1:q
  DAG[lower.tri(DAG)] = rbinom(n = q*(q-1)/2, size = 1, prob = w)
  return(DAG)
}
