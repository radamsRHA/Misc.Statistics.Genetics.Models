#
#JC69.Q <- function(lambda){
#	Q <- matrix(nrow = 4, ncol = 4)
#	rownames(Q) = colnames(Q) = c("T", "C", "A", "G")
#	Q[1,] <- c(-3*lambda, lambda, lambda, lambda)
#	Q[2,] <- c(lambda, -3*lambda, lambda, lambda)
#	Q[3,] <- c(lambda, lambda, -3*lambda, lambda)
#	Q[4,] <- c(lambda, lambda, lambda, -3*lambda)
#	return (Q)
#}
#
## Identity matrix
#I <- matrix(data = 0, nrow =4, ncol =4)
#diag(I) <- 1
#
#total.rate = 2.2*10^-9
#instant.rate = total.rate/3
#time <- 10^6
#
## If time is very small, when can compute the marix of transition probabilies:
#I+JC69.Q(lambda = instant.rate)*time
#
## If time is not very small, we must exponentiate the matrix:
#expm(JC69.Q(lambda = instant.rate)*time)
#
## Probability state in the descendent is the same as the ancestral state:
#p0 <- function(d){
#	p0 <- (1/4)+(3/4)*exp(-4*d/3)
#	return(p0)
#}
#
## Probability state in the descendent is a different specific state as the ancestral state:
#p1 <- function(d){
#	p1 <- (1/4)-(1/4)*exp(-4*d/3)
#	return(p1)
#}
#
#interv <- seq(from = 0, to = 2, by = 0.001)
#pTT <- rep(NA, length(interv))
#pTC <- rep(NA, length(interv))
#names(pTT) <- names(pTC) <- interv
#for (i in 1:length(interv)){
#	d = interv[i]
#	pTT[i] <- p0(d)
#	pTC[i] <- p1(d)
#}
#
#plot(pTT~as.numeric(names(pTT)), type = "l", xlim = c(0,2), ylim = c(0, 1))
#lines(pTC~as.numeric(names(pTC)), type = "l", col = "red")
#