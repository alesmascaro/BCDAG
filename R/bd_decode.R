#' Convert strings into matrices (internal function)
#'
#' This function restores matrices from string objects.
#' It is implemented in several functions, such as \code{get_causal_effect} and \code{get_edge_probs}, when output of \code{learn_DAG} was obtained with \code{save.memory = TRUE}
#'
#' @param string a string to convert into matrix
#' @param separator symbol used to separate elements in the input string
#'
#' @return The (q,q) original matrix from which the string vector was created using the internal function \code{bd_encode}
#'
#' @export

bd_decode <- function(string, separator = ";") {
  vec4mat <- as.numeric(strsplit(string, separator)[[1]])
  q <- length(vec4mat)
  matrix(vec4mat, ncol = sqrt(q))
}

