#matrix.1 <- matrix(nrow = 4, ncol = 4)
#rownames(matrix.1) <- colnames(matrix.1) <- c("A", "T", "C", "G")
#matrix.1[1,] <- c(1, 1, 1, 5)
#matrix.1[2,] <- c(3, 26, 6, 5)
#matrix.1[3,] <- c(1, 14, 33, 5)
#matrix.1[4,] <- c(4, 1, 0, 40)
#
## Matrix2 = matrix1/number of amino acids in the sequence
#matrix.2 <- round(matrix.1/146, digits = 3) # X(t)
#
## Matrix3  = x(0)
#matrix.3 <- matrix(nrow = 4, ncol = 4)
#rownames(matrix.3) <- colnames(matrix.3) <- c("A", "T", "C", "G")
#matrix.3[1,] <- c((round(sum(matrix.1[1,])/146, digits= 3)+ round(sum(matrix.1[,1])/146, digits= 3))/2, 0, 0, 0)
#matrix.3[2,] <- c(0, (round(sum(matrix.1[2,])/146, digits= 3)+ round(sum(matrix.1[,2])/146, digits= 3))/2, 0, 0)
#matrix.3[3,] <-c(0,0,(round(sum(matrix.1[3,])/146, digits= 3)+ round(sum(matrix.1[,3])/146, digits= 3))/2,0)
#matrix.3[4,] <- c(0,0,0,round((round(sum(matrix.1[4,])/146, digits = 3)+round(sum(matrix.1[,4])/146, digits = 3))/2, digits = 3))
#
## Matrix4 = inverse matrix3, X(0)^-1
#matrix.4 <- round(solve(matrix.3), digits = 3)
#
## Matrix5 = product of x(0)^-1 and X(t)
#matrix.5 <- round(matrix.4 %*%matrix.2, digits =6 )
#
## matrix 6 = eigenvalues of matrix.5
#matrix.6 <- eigen(matrix.5)$values
#
## matrix  = eigenvectors of matrix.5
#matrix.7 <- round(eigen(matrix.5)$vectors, digits = 3)
#
## matrix  = nverse of matrix.7
#matrix.8 <- round(solve(matrix.7), digits = 3)
#
## matrix.9, log of matrix.6``
#matrix.9 = log(matrix.6)
#
## marix.10 = log of matrix 5
#matrix.10 <- logm(matrix.5)
#
## matrix.11
#matrix.11 <- round(matrix.3 %*% logm(solve(matrix.3)%*%matrix.2), digits = 3)
#