#' Ok controllare det matrice una dimensione (dà errore)
#'
#' @param node
#' @param DAG
#' @param X
#' @param tXX
#' @param a
#' @param U
#'
#' @return
#' @export
#'
#' @examples

DW_nodemll <- function(node, DAG, X, tXX, a, U) {
  j <- node
  pa <- pa(j, DAG)

  n <- nrow(X)
  q <- ncol(X)

  a.star <- (a+length(pa)-q+1)


  Upost <- U + tXX

  if (length(pa) == 0) {
    U_jj <- U[j,j]
    Upost_jj <- Upost[j,j]
    #prior.normcost <- 1/gamma(a.star)*(1/2)*U_jj^a.star
    #post.normcost <- 1/gamma(a.star+n/2)*(1/2)*Upost_jj^(a.star+n/2)

    # occhio che sarebbe (1/2*U_jj)^a.star
    # di seguito calcolo già in log

    prior.normcost <- -lgamma(a.star/2) + a.star/2*log(U_jj/2)
    post.normcost <- -lgamma(a.star/2 + n/2) + (a.star/2 + n/2)*log(Upost_jj/2)

  } else {
    U_paj.j <- U[pa,j]
    U_jj <- U[j,j] - t(U_paj.j)%*%solve(U[pa,pa])%*%U_paj.j
    Upost_paj.j <- Upost[pa,j]
    Upost_jj <- Upost[j,j] - t(Upost_paj.j)%*%solve(Upost[pa,pa])%*%Upost_paj.j

    #prior.normcost <- 1/gamma(a.star)*(1/2)*U_jj^a.star*det(as.matrix(U[pa,pa]))^(1/2)
    #post.normcost <- 1/gamma(a.star+n/2)*(1/2)*(Upost_jj)^(a.star+n/2)*det((as.matrix(Upost[pa,pa])))^(1/2)

    prior.normcost <- -lgamma(a.star/2) + a.star/2*log(U_jj/2) + 0.5*log(det(as.matrix(U[pa,pa])))
    post.normcost <- -lgamma(a.star/2 + n/2) + (a.star/2 + n/2)*log(Upost_jj/2) + 0.5*log(det(as.matrix(Upost[pa,pa])))

  }

  #nodemll <- -n/2*log(2*pi) + log(prior.normcost) - log(post.normcost)
  nodemll <- -n/2*log(2*pi) + prior.normcost - post.normcost

  return(nodemll)
}
