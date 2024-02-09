#' Create new bcdagCE objects
#'
#' Internal function used as constructor for S3 objects of class \code{bcdagCE},
#' which constitute the output of function \code{get_causaleffect()}.
#'
#' @param x A list
#' @param input A list containing the inputs given to \code{learn_DAG()} and \code{get_causaleffect()}
#' @param type A string indicating whether the output produced by \code{learn_DAG()} was of type "complete", "compressed", "collapsed" or "compressed and collapsed"
#'
#' @return An S3 object of class \code{bcdagCE}
#' @noRd
#' @keywords internal
#'
#' @author Federico Castelletti and Alessandro Mascaro
#'
#' @references F. Castelletti and A. Mascaro (2022). BCDAG: An R package for Bayesian structural and Causal learning of Gaussian DAGs. \emph{arXiv pre-print}, url: https://arxiv.org/abs/2201.12003
new_bcdagCE <- function(x = list(), input = list(), type = "complete") {
  stopifnot(is.list(x))
  stopifnot(is.list(input))
  type <- match.arg(type, c("complete", "compressed", "collapsed", "compressed and collapsed"))

  structure(x,
            class = "bcdagCE",
            type = type,
            input = input)
}
