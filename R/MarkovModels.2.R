#################################
# JC69 instantanous rate matrix #
# (given instant rate)          #
#################################
Q.JC69 <- function(lambda){
  Q <- matrix(nrow = 4, ncol = 4)
  rownames(Q) <- colnames(Q) <- c("T", "C", "A", "G")
  Q[1,] <- c(-3*lambda, lambda, lambda, lambda)
  Q[2,] <- c(lambda, -3*lambda, lambda, lambda)
  Q[3,] <- c(lambda, lambda, -3*lambda, lambda)
  Q[4,] <- c(lambda, lambda, lambda, -3*lambda)
  return(Q)
}
#################################
# JC69 instantanous rate matrix #
# (given total rate             #
#################################
Q.JC69 <- function(total.rate){
  lambda = total.rate/3
  Q <- matrix(nrow = 4, ncol = 4)
  rownames(Q) <- colnames(Q) <- c("T", "C", "A", "G")
  Q[1,] <- c(-total.rate, lambda, lambda, lambda)
  Q[2,] <- c(lambda, -total.rate, lambda, lambda)
  Q[3,] <- c(lambda, lambda, -total.rate, lambda)
  Q[4,] <- c(lambda, lambda, lambda, -total.rate)
  return(Q)
}

#Q.JC69(lambda = 1)
#Q.JC69(total.rate = 3)


##############################################################
# Transition probability matrix for small time (t)           #
##############################################################

Pt <- function(total.rate, time){
  lambda = total.rate/3
  I <- matrix(0, nrow = 4, ncol = 4)
  diag(x = I) <- 1
  Q.matrix <- Q.JC69(total.rate = total.rate)
  Pt <- I + (Q.matrix*time)
  return(Pt)
}

#Pt(total.rate = 2.2*10^-9, time = 10^6)

##############################################################
# Transition probability matrix for large time (t)           #
##############################################################

Pt <- function(total.rate, time){
  lambda = total.rate/3
  p0 <- (1/4)+((3/4)*exp(-4*lambda*time))
  p1 <- (1/4)-((1/4)*exp(-4*lambda*time))
  Pt <- matrix(nrow = 4, ncol = 4)
  rownames(Pt) <- colnames(Pt) <- c("T", "C", "A", "G")
  Pt[1,] <- c(p0, p1, p1, p1)
  Pt[2,] <- c(p1, p0, p1, p1)
  Pt[3,] <- c(p1, p1, p0, p1)
  Pt[4,] <- c(p1, p1, p1, p0)
  return(Pt)
}

#Pt(total.rate = 2.2*10^-9, time = 10^6)
