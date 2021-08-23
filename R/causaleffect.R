#' Estimate the causal effect given a DAG and associated Cholesky parameters
#'
#' @param targets vector, targets of intervention
#' @param response numeric, response variable
#' @param DAG matrix, DAG needed to estimate the causal effect
#' @param L matrix, first Cholesky parameter
#' @param D matrix, second Cholesky parameter
#'
#' @return a vector of length |targets|
#' @export
#'
#' @examples
causaleffect <- function(targets, response, DAG, L, D){
  y <- response

  L_I <- L
  L_I[,targets] = 0
  diag(L_I) <- 1
  Sigma_I <- solve(t(L_I)) %*% D %*% solve(L_I)

  effects <- sapply(1:length(targets), function(x) Sigma_I[x,y]/Sigma_I[x,x])

  return(effects)
}
