n <- 1000
q <- 4

DAG <- matrix(c(0,1,1,0,0,0,0,1,0,0,0,1,0,0,0,0), nrow = q)

L <- DAG
L[L != 0] <- c(2,-1,3,-2)
diag(L) <- c(1,1,1,1)
D <- diag(1, q)
Sigma <- t(solve(L))%*%D%*%solve(L)

a <- 6
g <- 1/1000
U <- g*diag(1,q)
w = 0.2

causaleffect(4, 1, L = L, D = D)
causaleffect(c(4,2), 1, L = L, D = D)

test_that("causaleffect() estimates the correct causal effect", {
  expect_equal(causaleffect(4, 1, L = L, D = D), 8)
  expect_equal(causaleffect(c(4,2), 1, L = L, D = D), c(2,-2))
})

