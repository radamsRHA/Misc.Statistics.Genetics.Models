JC69 <- function(lambda){
  Q = matrix(ncol = 4, nrow = 4)
  Q[,1] <- c(-3*lambda, lambda, lambda, lambda)
  Q[,2] <- c(lambda, -3*lambda, lambda, lambda)
  Q[,3] <- c(lambda, lambda, -3*lambda, lambda)
  Q[,4] <- c(lambda, lambda, lambda, -3*lambda)
  colnames(Q) = c("T", "C", "A", "G")
  rownames(Q) = c("T", "C", "A", "G")
  return(Q)
}

JC69.smallt <- function(lambda, t){
  Pt = matrix(ncol = 4, nrow = 4)
  Pt[,1] <- c(1-3*lambda*t, lambda*t, lambda*t, lambda*t)
  Pt[,2] <- c(lambda*t, 1-3*lambda*t, lambda*t, lambda*t)
  Pt[,3] <- c(lambda*t, lambda*t, 1-3*lambda*t, lambda*t)
  Pt[,4] <- c(lambda*t, lambda*t, lambda*t, 1-3*lambda*t)
  colnames(Pt) = c("T", "C", "A", "G")
  rownames(Pt) = c("T", "C", "A", "G")
  return(Pt)
}

JC69.larget <- function(lambda, t){
  P0.t <- (1/4)+((3/4)*exp(-4*lambda*t))
  P1.t <- (1/4)-((1/4)*exp(-4*lambda*t))
  Pt = matrix(ncol = 4, nrow = 4)
  Pt[,1] <- c(P0.t, P1.t, P1.t, P1.t)
  Pt[,2] <- c(P1.t, P0.t, P1.t, P1.t)
  Pt[,3] <- c(P1.t, P1.t, P0.t, P1.t)
  Pt[,4] <- c(P1.t, P1.t, P1.t, P0.t)
  colnames(Pt) = c("T", "C", "A", "G")
  rownames(Pt) = c("T", "C", "A", "G")
  return(Pt)
}


####################################
## Compare large vs. small t #######
####################################
#
#subrate <- 2.2*10^-9
#lambda <- subrate/3
#smallt.estimate <- JC69.smallt(lambda, 10^8)
#larget.estimate <- JC69.larget(lambda, 0)
#
##print("difference in rate estimates, pT->C is:")
##smallt.estimate[1,2] - larget.estimate[1,2]
#
####################################
## Evolve sequence for t gen #######
####################################
#
#sequence = c("A", "A", "A", "T", "A", "G")
#newsequence <- vector()
#for (i in sequence){
#  probt <- larget.estimate[i, ]
#  states <- c("T", "C", "A", "G")
#  newnt <- sample(states, size = 1, replace = T, prob = probt)
#  newsequence <- c(newsequence, newnt)
#}
#newsequence
#
####################################
## Rate & time are confounded ######
####################################
#
#lambda = .1
#t = .9
#JC69.larget(lambda, t)
#
#lambda = .9
#t = .1
#JC69.larget(lambda, t)
#
### same transition probabilties when lambda = .9 or lambda = .1, or t = .1 or t = .9
#
####################################
## P0 vs. P1 given true distance, d 
####################################
#
#ptt <- vector()
#ptc <- vector()
#subrate <- 2.2*10^-9
#lambda <- subrate/3
#for (d in seq(0, 2, .01)){
#  t = d/(3*lambda)
#  P0.t <- (1/4)+((3/4)*exp(-4*lambda*t))
#  P1.t <- (1/4)-((1/4)*exp(-4*lambda*t))
#  ptt <- c(ptt, P0.t)
#  ptc <- c(ptc, P1.t)
#}
#
## Both ptt and ptc convergence to 1/4 across different genetic distances d
#
####################################
## Pij convergece to 1/4, t -> limit infinity 
####################################
#
#JC69.larget(lambda, 10^1000)
#
#
#
#