K80 <- function (alpha, beta){
    Q = matrix(ncol = 4, nrow = 4)
    Q[,1] <- c(-(alpha+2*beta), alpha, beta, beta)
    Q[,2] <- c(alpha, -(alpha+2*beta), beta, beta)
    Q[,3] <- c(beta, beta, -(alpha+2*beta), alpha)
    Q[,4] <- c(beta, beta, alpha, -(alpha+2*beta))
    colnames(Q) = c("T", "C", "A", "G")
    rownames(Q) = c("T", "C", "A", "G")
    return(Q)
}

K80<- function(alpha, beta, t){
  d = (alpha+2*beta)/t
  k = alpha/beta
  P0.t <- (1/4)+((1/4)*exp((-4*d)/(k+2)))+((1/2)*exp((-2*d*(k+1))/(k+2)))
  P0.t <- (1/4)+((1/4)*exp((-4*d)/(k+2)))-((1/2)*exp((-2*d*(k+1))/(k+2)))
  P2.t <- (1/4)-((1/4)*exp(-4*d/(k+2)))
  Pt = matrix(ncol = 4, nrow = 4)
  Pt[,1] <- c(P0.t, P1.t, P2.t, P2.t)
  Pt[,2] <- c(P1.t, P0.t, P2.t, P2.t)
  Pt[,3] <- c(P2.t, P2.t, P0.t, P1.t)
  Pt[,4] <- c(P2.t, P2.t, P1.t, P0.t)
  colnames(Pt) = c("T", "C", "A", "G")
  rownames(Pt) = c("T", "C", "A", "G")
  return(Pt)
}

dhat <- function(S, V){
  d.hat <- (-1/2)*log(1-2*S-V)-(1/4)*log(1-2*V)
  return(d.hat)
}

khat <- function(S, V){
  k.hat <- 2*log(1-2*S-V)
  k.hat <- k.hat/log(1-2*V)
  k.hat <- k.hat -1
  return(k.hat)
}

#S = 0.08861
#V = 0.00633
#dhat(S, V)
#khat(S, V)
#