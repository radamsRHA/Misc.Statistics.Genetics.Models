#library(expm)
#
### P = single-step transition probability matrix
##P <- matrix(nrow  = 2, ncol = 2)
##P[1,] <- c(0.90, 0.10)
##P[2,] <- c(0.25, 0.75)
##rownames(P) <- c("state1", "state2")
##colnames(P) <- c("state1", "state2")
##
### Pt = raise P (single-step) to the power of t for more steps
##Pt <- P %^% 2
##
### t = 5
##Pt <- P %^% 5
##
### t = 20
##Pt <- P %^% 20
##
### It takes an average of 10 steps to leave state 1 for state 2 
### It takes an average of 4 steps to leave state 2 for state 1
### v1 = 10/14 = 0.714 and v2 = 4/14 = 0.28
##
########################
## Bernoulli P matrix
########################
#Bern.P <- function(p){
#	P <- matrix(nrow = 2, ncol = 2)
#	P[1,] <- c((1-p), p)
#	P[2,] <- c((1-p), p)
#	rownames(P) <- c("state1", "state2")
#	colnames(P) <- c("state1", "state2")
#	return(P)
#}
#
### Bern.P = P^2 = P^20
##Bern.P(p = 0.25)
##Bern.P(p = 0.25)%^% 2
##Bern.P(p = 0.25)%^% 20
##
########################
## Solve the equation v=vP
########################
#
#v = (P%^%100000)[1,]
#v%*%P 
#
########################
## P with absorbing states
########################
#
#abs.P <- function(p.21, p.23){
#	P <- matrix(nrow = 3, ncol = 3)
#	P[1,] <- c(1, 0,0)
#	P[2,] <- c(p.21, (1-p.21-p.23), p.23)
#	P[3,] <- c(0,0,1)
#	rownames(P) <- c("state1", "state2", "state3")
#	colnames(P) <- c("state1", "state2", "state3")
#	return(P)
#}
#
#
#P <- abs.P(p.21 = 1/6, p.23 = 2/3)
#P%^%1
#
#
#(1/3)/(1/3+1/6)
#
#