#Q.matrix <- function(lambda){
#Q <- matrix(ncol = 4, nrow = 4)
#rownames(Q) <- colnames(Q) <- c("A", "C", "G", "T")
#Q[1,] <- c(-3*lambda, lambda, lambda, lambda)
#Q[2,] <- c(lambda, -3*lambda, lambda, lambda)
#Q[3,] <- c(lambda, lambda, -3*lambda, lambda)
#Q[4,] <- c(lambda, lambda, lambda, -3*lambda)
#return(Q)
#}
#
#
#
#Q <- Q.matrix(lambda = (2.2*10^-9)/3)
#LAM <- matrix(nrow = 4, ncol = 4)
#diag(LAM) <- eigen(Q)$values
#LAM[1,2:4] <- LAM[2,1] <- LAM[2,3:4] <- LAM[3,1:2] <- LAM[3,4] <- LAM[4,1:3] <- 0
#U <- eigen(Q)$vectors
#U.1 <- solve(U)
#U %*%LAM %*%U.1
#Q
#