n <- 10000
q <- 4

set.seed(1)
DAG <- matrix(c(0,1,1,0,0,0,0,0,0,0,0,0,0,1,1,0), nrow = q)

L <- DAG
L[L != 0] <- runif(q, 0.2, 1)
diag(L) <- c(1,1,1,1)
D <- diag(1, q)
Sigma <- t(solve(L))%*%D%*%solve(L)

a <- 6
g <- 1/1000
U <- g*diag(1,q)
w = 0.2

X <- mvtnorm::rmvnorm(n, sigma = Sigma)

out <- learn_DAG(1000, 0, X, a, U, w, fast = TRUE, collapse = TRUE)


# Wrong input -------------------------------------------------------------

outw <- list(Graphs = c(1,2), L = c(1,2), D = c(1,1))

wrongOut <- "get_causaleffect(outw, c(2,3), 1)"

test_that("learn_DAG identifies the correct DAG in a simple case", {
  expect_error(eval(parse(text = wrongOut)), "learnDAG_output must be an object of class bcdag")
})
