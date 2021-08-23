bd_decode <- function(string, separator = ";") {
  vec4mat <- as.numeric(strsplit(string, separator)[[1]])
  q <- length(vec4mat)
  matrix(vec4mat, ncol = q)
}
