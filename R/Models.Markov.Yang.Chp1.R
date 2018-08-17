#library(expm)
#library(psych)
#
## instantaneous substitution rate matrix
#Q <- function(instant.rate){
#	Q <- matrix(nrow = 4, ncol = 4)
#	Q[1,] <- c(-3*instant.rate, instant.rate, instant.rate, instant.rate)
#	Q[2,] <- c(instant.rate, -3*instant.rate, instant.rate, instant.rate)
#	Q[3,] <- c(instant.rate, instant.rate, -3*instant.rate, instant.rate)
#	Q[4,] <- c(instant.rate, instant.rate, instant.rate, -3*instant.rate)
#	rownames(Q) <- colnames(Q) <- c("T", "C", "A", "G")
#	return(Q)
#}
#
## total.rate = 3*instant.rate
#total.rate = 2.2*10^-9
#time = 10^6
#expm(Q(instant.rate = total.rate/3)*time)
#
## probability that a site in state T will be in state T after 10^6 years:
#P <-expm(Q(instant.rate = total.rate/3)*time) 
#P["T","T"]
#(1-total.rate*time)
#
## probability that a site in state T will be in state C after 10^6 years:
#P <-expm(Q(instant.rate = total.rate/3)*time) 
#P["T","C"]	
#
###################
## Compute P matrix 
###################
#
#I <- matrix(nrow = 4, ncol = 4)
#I[1,] <- c(1,0,0,0)
#I[2,] <- c(0,1,0,0)
#I[3,] <- c(0,0,1,0)
#I[4,] <- c(0,0,0,1)
#
## if Time is small = 
#I+Q(instant.rate = total.rate/3)*time
#