n <- 100
q <- 4

DAG1 <- matrix(c(0,1,1,0,0,0,0,1,0,0,0,1,0,0,0,0), nrow = q)
DAG2 <- matrix(c(0,1,1,0,0,0,0,0,0,0,0,1,0,1,0,0), nrow = q)
DAG3 <- matrix(c(0,1,1,0,0,0,0,0,0,0,0,0,0,1,1,0), nrow = q)

a <- 6
g <- 1/100
U <- g*diag(1,q)

rDAGWishart(1, DAG3, a, U)
rDAGWishart(10, DAG3, a, U)
rDAGWishart(100, DAG3, a, U)

rDAGWishart(10, DAG3, a, U)$D
rDAGWishart(100, DAG3, a, U)$D

rDAGWishart(10, DAG3, a, U)$L
rDAGWishart(100, DAG3, a, U)$L

