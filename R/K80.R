####################################################################
################# Substitution rate matrix Q########################
####################################################################

K80.Q <- function(alpha, beta){
  Q <- matrix(nrow = 4, ncol = 4)
  Q[,1] <- c(-(alpha+2*beta), alpha, beta, beta)
  Q[,2] <- c(alpha, -(alpha+2*beta), beta, beta)
  Q[,3] <- c(beta, beta, -(alpha+2*beta), alpha)
  Q[,4] <- c(beta, beta, alpha, -(alpha+2*beta))
  colnames(Q) <- c("T", "C", "A", "G")
  rownames(Q) <- c("T", "C", "A", "G")
  return(Q)
}

#### In terms of R and k
K80.Q <- function(R, K){
  Q <- matrix(nrow = 4, ncol = 4)
  Q[,1] <- c(-R, (K*R)/(K+2), R/(K+2), R/(K+2))
  Q[,2] <- c((K*R)/(K+2), -R, R/(K+2), R/(K+2))
  Q[,3] <- c(R/(K+2), R/(K+2), -R, (K*R)/(K+2))
  Q[,4] <- c(R/(K+2), R/(K+2), (K*R)/(K+2), -R)
  colnames(Q) <- c("T", "C", "A", "G")
  rownames(Q) <- c("T", "C", "A", "G")
  return(Q)
}

####################################################################
################# Transitional Probality Matrix; t << ##############
####################################################################

K80.Pt <- function(d, k){
  Pt <- matrix(nrow = 4, ncol = 4)
  p0 <- (1/4)+((1/4)*exp(-4*d/(k+2)))+((1/2)*exp(-2*d*(k+1)/(k+2)))
  p1 <- (1/4)+((1/4)*exp(-4*d/(k+2)))-((1/2)*exp(-2*d*(k+1)/(k+2)))
  p2 <- (1/4)-((1/4)*exp(-4*d/(k+2)))
  Pt[,1] <- c(p0, p1, p2, p2)
  Pt[,2] <- c(p1, p0, p2, p2)
  Pt[,3] <- c(p2, p2, p0, p1)
  Pt[,4] <- c(p2, p2, p1, p0)
  colnames(Pt) <- c("T", "C", "A", "G")
  rownames(Pt) <- c("T", "C", "A", "G")
  return(Pt)
}

####################################################################
################# Estimate k, d ####################################
####################################################################

estimate.d <- function(S, V){
  d.hat <- -(1/2)*log(1-2*S-V) - (1/4)*log(1-2*V)
  return(d.hat)
}

estimate.k <- function(S, V){
  k.hat <- ((2*log(1-(2*S)-V))/(log(1-(2*V))))-1
  return(k.hat)
}

#estimate.k(.08861, .00633)

####################################################################
################# Estimate with rate variation #####################
####################################################################

estimate.d <- function(alpha, S, V){
  t1 <- (((1-2*S-V)^(-1/alpha))-1)
  t2 <- ((1-2*V)^(-1/alpha)-1)
  d.hat <- ((alpha/2)*t1) + (alpha/4)*(t2)
  return(d.hat)
}

estimate.k <- function(alpha, S, V){
  t1 <- 2*(((1-2*S-V)^(-1/alpha))-1)
  t2 <- ((1-2*V)^(-1/alpha))-1
  k.hat <- t1/t2 -1
  return(k.hat)
}

# EXAMPLE
#estimate.d(.5, .08861, 0.00633)
#estimate.k(.5, .08861, 0.00633)
