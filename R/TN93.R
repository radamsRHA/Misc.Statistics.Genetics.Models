####################################################################
################# Substitution rate matrix Q########################
####################################################################

TN93.Q <- function(alpha1, alpha2, beta, pi.vec){
  a1 <- alpha1
  a2 <- alpha2
  b <- beta
  pi.T <- pi.vec["T"]
  pi.C <- pi.vec["C"]
  pi.A <- pi.vec["A"]
  pi.G <- pi.vec["G"]
  pi.R <- pi.A + pi.G
  pi.Y <- pi.T + pi.C
  Q <- matrix(nrow = 4, ncol = 4)
  Q[,1] <- c(-(a1*pi.C+b*pi.R), a1*pi.C, b*pi.A, b*pi.G)
  Q[,2] <- c(a1*pi.T, -(a1*pi.T+b*pi.R), b*pi.A, b*pi.G)
  Q[,3] <- c(b*pi.T, b*pi.C, -(a2*pi.G+b*pi.Y), a2*pi.G)
  Q[,4] <- c(b*pi.T, b*pi.C, a2*pi.A, -(a2*pi.A+b*pi.Y))
  colnames(Q) <- c("T", "C", "A", "G")
  rownames(Q) <- c("T", "C", "A", "G")
  return(Q)
}

#sum(TN93.Q(.1, .1, .2, pi)[,1])
