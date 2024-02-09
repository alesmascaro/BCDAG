#' Create new bcdag objects
#'
#' Internal function used as constructor for S3 objects of class \code{bcdag}, which constitute the output of function \code{learn_DAG()} and the input of functions belonging to the get_ family such as \code{get_causaleffect()}.
#'
#' @param x A list
#' @param input A list containing the inputs given to \code{learn_DAG()}
#' @param type A string indicating whether the output produced by \code{learn_DAG()} should be of type "complete", "compressed", "collapsed" or "compressed and collapsed"
#'
#' @return An S3 object of class \code{bcdag}
#' @noRd
#' @keywords internal
#'
#' @author Federico Castelletti and Alessandro Mascaro
#'
#' @references F. Castelletti and A. Mascaro (2022). BCDAG: An R package for Bayesian structural and Causal learning of Gaussian DAGs. \emph{arXiv pre-print}, url: https://arxiv.org/abs/2201.12003
new_bcdag <- function(x = list(), input = list(), type = "complete") {
  stopifnot(is.list(x))
  stopifnot(is.list(input))
  type <- match.arg(type, c("complete", "compressed", "collapsed", "compressed and collapsed"))

  structure(x,
            class = "bcdag",
            type = type,
            input = input)
}
