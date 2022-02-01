n <- 10000
q <- 4

DAG1 <- matrix(c(0,1,1,0,0,0,0,1,0,0,0,1,0,0,0,0), nrow = q)
DAG2 <- matrix(c(0,1,1,0,0,0,0,0,0,0,0,1,0,1,0,0), nrow = q)
DAG3 <- matrix(c(0,1,1,0,0,0,0,0,0,0,0,0,0,1,1,0), nrow = q)

L <- DAG3
L[L != 0] <- runif(q, 0.2, 1)
diag(L) <- c(1,1,1,1)
D <- diag(1, q)
Sigma <- t(solve(L))%*%D%*%solve(L)

a <- 6
g <- 1/1000
U <- g*diag(1,q)
w = 0.2

set.seed(1)
X <- mvtnorm::rmvnorm(n, sigma = Sigma)

out <- learn_DAG(1000, 0, X, a, U, w, fast = TRUE, collapse = TRUE, save.memory = FALSE)

test_that("learn_DAG identifies the correct DAG in a simple case", {
  expect_equal(bd_encode(DAG3), bd_encode(get_MPMdag(out)))
})

