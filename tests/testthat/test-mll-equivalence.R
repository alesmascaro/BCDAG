n <- 100
q <- 4

DAG1 <- matrix(c(0,1,1,0,0,0,0,1,0,0,0,1,0,0,0,0), nrow = q)
DAG2 <- matrix(c(0,1,1,0,0,0,0,0,0,0,0,1,0,1,0,0), nrow = q)
DAG3 <- matrix(c(0,1,1,0,0,0,0,0,0,0,0,0,0,1,1,0), nrow = q)

L <- DAG3
L[L != 0] <- runif(q, 0.2, 1)
diag(L) <- c(1,1,1,1)
D <- diag(1, q)
Sigma <- t(solve(L))%*%D%*%solve(L)


X <- mvtnorm::rmvnorm(n, sigma = Sigma)
tXX <- t(X)%*%X

a <- 6
g <- 1/n
U <- g*diag(1,q)

test_that("Score equivalence of Markov equivalent DAGs", {
  expect_equal(sum(sapply(1:q, function(j) DW_nodemll(j, DAG1, X, tXX, a, U))),
               sum(sapply(1:q, function(j) DW_nodemll(j, DAG2, X, tXX, a, U))))
})

test_that("Score difference of non-Markov equivalent DAGs", {
  expect_false(sum(sapply(1:q, function(j) DW_nodemll(j, DAG1, X, tXX, a, U))) ==
                 sum(sapply(1:q, function(j) DW_nodemll(j, DAG3, X, tXX, a, U))))
})
