n <- 1000
q <- 4

DAG <- matrix(c(0,1,1,0,0,0,0,0,0,0,0,0,0,1,1,0), nrow = q)

L <- DAG
L[L != 0] <- runif(q, 0.2, 1)
diag(L) <- c(1,1,1,1)
D <- diag(1, q)
Sigma <- t(solve(L))%*%D%*%solve(L)

set.seed(1)
X <- mvtnorm::rmvnorm(n, sigma = Sigma)

a <- 6
g <- 1/1000
U <- g*diag(1,q)
w = 0.2


# Wrong S or burn ---------------------------------------------------------

wrongS <- "learn_DAG(c(2000, 1000), 0, data = X, a, U, w, fast = TRUE, collapse = TRUE)"
wrongBurn <- "learn_DAG(2000, c(2,3), data = X, a, U, w, fast = TRUE, collapse = TRUE)"

test_that("An error message is returned when wrong S or burn are given", {
  expect_error(eval(parse(text = wrongS)),
               "S and burn must be integer numbers")
  expect_error(eval(parse(text = wrongBurn)),
               "S and burn must be integer numbers")
})


# Wrong a -----------------------------------------------------------------

wrongA <- "learn_DAG(1000, 0, X, a = 3, U, w, fast = TRUE, collapse = TRUE)"

test_that("An error message is returned when a is too small", {
  expect_error(eval(parse(text = wrongA)),
               "a must be at least equal to the number of variables")
})


# Wrong U -----------------------------------------------------------------

Uw <- matrix(1:16, ncol = 4)
Ui <- matrix(c(2,3,1,4,5,2,3,5,1), ncol = 3)

wrongUw <- "learn_DAG(1000, 0, X, a = a, U = Uw, w, fast = TRUE, collapse = TRUE)"
wrongUi <- "learn_DAG(1000, 0, X, a = a, U = Ui, w, fast = TRUE, collapse = TRUE)"

test_that("An error message is returned when wrong U is given", {
  expect_error(eval(parse(text = wrongUw)),
               "U must be a squared symmetric positive definite matrix")
  expect_error(eval(parse(text = wrongUi)),
               "U must be a squared symmetric positive definite matrix")
})


# Wrong w -----------------------------------------------------------------

w1 <- 1.2
w2 <- "a"

wrongW1 <- "learn_DAG(1000, 0, X, a = a, U = U, w = w1, fast = TRUE, collapse = TRUE)"
wrongW2 <- "learn_DAG(1000, 0, X, a = a, U = U, w = w2, fast = TRUE, collapse = TRUE)"

test_that("An error message is returned when wrong w is given", {
  expect_error(eval(parse(text = wrongW1)),
               "w must be a number between 0 and 1")
  expect_error(eval(parse(text = wrongW2)),
               "w must be a number between 0 and 1")
})
