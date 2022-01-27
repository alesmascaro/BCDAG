#' Convert matrix into strings (internal function)
#'
#' This function converts matrices into string objects.
#' It is implemented in \code{learn_DAG} when \code{save.memory = TRUE}
#'
#' @param matrix a matrix to convert into string
#' @param separator symbol used to separate elements of the matrix in the string
bd_encode <- function(matrix, separator = ";") {
  paste(matrix, collapse = separator)
}
