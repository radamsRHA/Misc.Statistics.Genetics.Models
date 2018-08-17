#library(expm)
#
#Q.scaled <- function(pi.green, pi.red){
#	mu = 1/(2*pi.green*pi.red) # Scaled mutation rate, such that divergence is measured in expeted number of substituions
#	Q <- matrix(nrow = 2, ncol = 2)
#	Q[1,] <- c(-pi.green, pi.green)
#	Q[2,] <- c(pi.red, -pi.red)
#	rownames(Q) = colnames(Q) = c("green", "red")
#	print(mu)
#	return(Q*mu)
#}
#
#Q.scaled(pi.green = 0.5, pi.red = 0.5)
#
#
#pv <- function(Q, v){ # Compute matrix expontial to get transition probabilites for a given time (v)
#	p.v <- expm(Q*v)
#	return(p.v)
#}
#
#p.v <- pv(Q = Q.scaled(pi.green = 0.5, pi.red = 0.5), v = 0.1)
#lnl <- (1/2*p.v[1,1]*p.v[1,2])+(1/2*p.v[2,2]*p.v[2,1])
#