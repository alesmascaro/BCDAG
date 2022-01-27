new_bcdag <- function(x = list(), input = list(), type = "complete") {
  stopifnot(is.list(x))
  stopifnot(is.list(input))
  type <- match.arg(type, c("complete", "compressed", "collapsed", "compressed and collapsed"))

  structure(x,
            class = "bcdag",
            type = type,
            input = input)
}
