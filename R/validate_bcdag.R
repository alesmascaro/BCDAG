validate_bcdag <- function(bcdag) {
  if (class(bcdag) != "bcdag") stop("learnDAG_output must be an object of class bcdag")
  values <- unclass(bcdag)
  input <- attr(bcdag, "input")
  type <- attr(bcdag, "type")

  q <- ncol(input$data)

  if (type == "complete") {
    if (length(values) != 3 & !is.array(values$Graphs) & !is.array(values$L) & !is.array(values$D) &
        all(sapply(list(dim(values$Graphs), dim(values$L)), function(x) x == dim(values$D)))) {
      stop(
        "A complete bcdag object must contain a list of three qxqxS arrays",
        call. = FALSE
      )
    }
  }

  if (type == "compressed") {
    if (length(values) != 3 & !is.character(values$Graphs) & !is.character(values$L) &
        !is.character(values$D) &
        all(sapply(list(length(values$Graphs), length(values$L)), function(x) x == length(values$D)))) {
      stop(
        "A compressed bcdag object must contain a list of three characters vectors of length S",
        call. = FALSE
      )
    }
  }

  if (type == "collapsed") {
    if (length(values) != 1 & !is.array(values$Graphs) &
        all(dim(values$Graphs) == c(q, q, input$S))) {
          stop(
            "A collapsed bcdag object must contain only a qxqxS array of sampled DAGs",
            call. = FALSE
          )
        }
  }
  if (type == "compressed and collapsed") {
    if (length(values) != 1 & !is.character(values$Graphs) & length(values$Graphs) == input$S) {
      stop(
        "A compressed and collapsed bcdag object must contain only a character vector of length S",
        call. = FALSE
      )
    }
  }
  TRUE
}
