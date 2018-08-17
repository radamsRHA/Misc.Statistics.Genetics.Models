#####################################################
# gamma distribution: the waiting time until the nth event is a sum of n random variables with expoentnial waiting times
#####################################################

Gamma.Dist <- function(lambda, W, n){
	t1 <- lambda*exp(-lambda*W)
	t2 <- (lambda*W)^(n-1)
	t3 <- factorial(n-1)
	fW <- t1*t2/t3
	print(fW)
}

#Gamma.Dist(lambda = 0.5, W = 10, n = 10)
#dgamma(x = 10, shape =5)

#####################################################
# pY = k, the sum of independent poissons
#####################################################

pY <- function(lambda1, lambda2, k){
	t1 <- exp(-lambda1+lambda2)/factorial(k)
	t2 <- (lambda1+lambda2)^k
	pk <- t1*t2
	print(pk)
}

#####################################################
# Convolutions of exponential distribtions
# lamda.1 = rate of process between 0th and 1th event
# lamda.2 = rate of process between 1th and 2th event
#####################################################

fT1.T2 <- function(lambda.1, lambda.2, t){
	t1 <- (lambda.1)/(lambda.1-lambda.2) *lambda.2 *exp(-lambda.2*t)
	t2 <- (lambda.2)/(lambda.2-lambda.1) *lambda.1 *exp(-lambda.1*t)
	ft <- t1+t2
	print(ft)
}
