n <- 1000
q <- 4

DAG <- matrix(c(0,1,1,0,0,0,0,1,0,0,0,1,0,0,0,0), nrow = q)

L <- DAG
L[L != 0] <- c(2,-1,3,-2)
diag(L) <- c(1,1,1,1)
D <- diag(1, q)
Sigma <- t(solve(L))%*%D%*%solve(L)


# Wrong targets input -----------------------------------------------------

wrongT1 <- "causaleffect(\"a\",1, L = L, D = D)"
wrongT2 <- "causaleffect(c(\"a\",2), 1, L = L, D = D)"

test_that("causaleffect() returns error when targets are not correctly specified", {
  expect_error(eval(parse(text = wrongT1)),
               "targets must be a vector containing the position of intervention targets in the dataset")
  expect_error(eval(parse(text = wrongT2)),
               "targets must be a vector containing the position of intervention targets in the dataset")
})


# Repeated targets --------------------------------------------------------

repeatedT <- "causaleffect(c(2,2), 1, L = L, D = D)"

test_that("causaleffect() from causal effects when the targets indicated are not unique", {
  expect_warning(eval(parse(text = repeatedT)),
                 "Your vector of targets does not contain distinct elements")
})


# Wrong response ----------------------------------------------------------

wrongR1 <- "causaleffect(2, \"a\", L = L, D = D)"
wrongR2 <- "causaleffect(2, 1.4, L = L, D = D)"

test_that("causaleffect() returns error when response is not correctly specified", {
  expect_error(eval(parse(text = wrongR1)),
               "response must be the numerical value indicating the position of the response variable in the dataset")
  expect_error(eval(parse(text = wrongR2)),
               "response must be the numerical value indicating the position of the response variable in the dataset")
})


# L not a DAG -------------------------------------------------------------

DAGw <- matrix(c(0,0,1,0,1,0,0,0,0,0,0,1,0,1,0,0), nrow = q)

Lw <- DAGw
Lw[Lw != 0] <- c(2,-1,3,-2)
diag(Lw) <- c(1,1,1,1)
Sigmaw <- t(solve(Lw))%*%D%*%solve(Lw)

wrongDAG <- "causaleffect(3, 1, L = Lw, D = D)"

test_that("causaleffect() returns error when L is not from a DAG", {
  expect_error(eval(parse(text = wrongDAG)),
               "L is not a matrix of coefficients of an acyclic SEM")
})


# Wrong D -----------------------------------------------------------------

Dw <- diag(-1, q)

wrongD <- "causaleffect(3, 1, L = L, D = Dw)"

test_that("causaleffect() returns error when L is not from a DAG", {
  expect_error(eval(parse(text = wrongD)),
               "D must be a qxq diagonal matrix of conditional variance parameters")
})

