####################################################################
################# Substitution rate matrix Q########################
####################################################################

JC69.Q <- function(instant.rate){
  total.sub.rate <- 3*instant.rate
  Q <- matrix(nrow = 4, ncol = 4)
  Q[,1] <- c(-3*instant.rate, instant.rate, instant.rate, instant.rate)
  Q[,2] <- c(instant.rate, -3*instant.rate, instant.rate, instant.rate)
  Q[,3] <- c(instant.rate, instant.rate, -3*instant.rate, instant.rate)
  Q[,4] <- c(instant.rate, instant.rate, instant.rate, -3*instant.rate)
  colnames(Q) <- c("T", "C", "A", "G")
  rownames(Q) <- c("T", "C", "A", "G")
  return(Q)
}

####################################################################
################# Transitional Probality Matrix; t << ##############
####################################################################

JC69.Pt.s <- function(sub.rate, t){
  instant.rate <- sub.rate/3
  Pt <- matrix(nrow = 4, ncol = 4)
  Pt[,1] <- c(1-3*instant.rate*t, instant.rate*t, instant.rate*t, instant.rate*t)
  Pt[,2] <- c(instant.rate*t, 1-3*instant.rate*t, instant.rate*t, instant.rate*t)
  Pt[,3] <- c(instant.rate*t, instant.rate*t, 1-3*instant.rate*t, instant.rate*t)
  Pt[,4] <- c(instant.rate*t, instant.rate*t, instant.rate*t, 1-3*instant.rate*t)
  colnames(Pt) <- c("T", "C", "A", "G")
  rownames(Pt) <- c("T", "C", "A", "G")
  return(Pt)
}

# Example
#sub.rate <- 2.2*10^-9
#time <- 10^6
#JC69.Pt.s(sub.rate, time)

####################################################################
################# Transitional Probality Matrix; t >> ##############
####################################################################

JC69.Pt<- function(sub.rate, t){
  instant.rate <- sub.rate/3
  p0 <- (1/4)+(3/4)*exp(-4*instant.rate*t)
  p1 <- (1/4)-(1/4)*exp(-4*instant.rate*t)
  Pt <- matrix(nrow = 4, ncol = 4)
  Pt[,1] <- c(p0, p1, p1, p1)
  Pt[,2] <- c(p1, p0, p1, p1)
  Pt[,3] <- c(p1, p1, p0, p1)
  Pt[,4] <- c(p1, p1, p1, p0)
  colnames(Pt) <- c("T", "C", "A", "G")
  rownames(Pt) <- c("T", "C", "A", "G")
  return(Pt)
}

# EXAMPLE
#sub.rate <- 2.2*10^-9
#time <- 10^6
#JC69.Pt(sub.rate, time)

# rate and time counfounded
# JC69.Pt(sub.rate/3, time*3)
# JC69.Pt(sub.rate*3, time/3)

####################################################################
################# Evolve Sequence via JC60.Pt(large)  ##############
####################################################################

Evolve <- function(sequence, sub.rate, time){
  seq.t <- vector()
  for (site in 1:length(sequence)){
    nuc <- sequence[site]
    P.matrix <- JC69.Pt(sub.rate, time)
    p.row <- P.matrix[,nuc]
    new.nt <- sample(c("T", "C", "A", "G"), size = 1, prob = p.row)
    seq.t <- c(seq.t, new.nt)
  }
  print(seq.t)
}

# EXAMPLE
# Evolve(c("T", "T", "T"), sub.rate, 0)

####################################################################
################# Transitional Probality Matrix; t >>; d  ##########
####################################################################

JC69.Pt<- function(d){
  p0 <- (1/4)+(3/4)*exp(-4*d/3)
  p1 <- (1/4)-(1/4)*exp(-4*d/3)
  Pt <- matrix(nrow = 4, ncol = 4)
  Pt[,1] <- c(p0, p1, p1, p1)
  Pt[,2] <- c(p1, p0, p1, p1)
  Pt[,3] <- c(p1, p1, p0, p1)
  Pt[,4] <- c(p1, p1, p1, p0)
  colnames(Pt) <- c("T", "C", "A", "G")
  rownames(Pt) <- c("T", "C", "A", "G")
  return(Pt)
}


####################################################################
################# Estimate genetic distance, d  ####################
####################################################################
# p(d; site differs) <- 3*p1(t) = 3*(1/4-1/4*exp(-4d/3)) <- 3/4 -3/4exp(-4d/3)

estimate.d <- function(p.hat){
  d.hat <- -3/4*log(1-(4/3)*p.hat)
  return(d.hat)
}
#estimate.d(.09494)

####################################################################
################# Estimate genetic distance, d; with rate variation
####################################################################

estimate.d <- function(alpha, p.hat){
  t1 <- (1-(4/3)*p.hat)^(-1/alpha)
  t2 <- t1 - 1
  d.hat <- (3/4)*alpha*t2
  return(d.hat)
}

#estimate.d(0.5, .09494)
