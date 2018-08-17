######################################################
## Discrete Geometric distribution
######################################################
#
#pT.t.discrete <- function(max.T, p, t.discrete){
#	geometric.dist <- Geom.Dist(max.T, p)
#	pt <- geometric.dist[t.discrete]
#	print(pt)
#}
#
#
## probability of t in discrete time
#pT.t.discrete(max.T = 10, p = 0.5, t.discrete = 10)
#
#
######################################################
## rescaled Geometric distribution
######################################################
#pT.t.cont <- function(lambda, t.discrete, delta.t){
#	new.p = lambda*delta.t
#	t.cont = t.discrete/delta.t
#	pT = new.p*(1-new.p)^t.cont
#	print(pT)
#}
#
## rescale time by setting the previos 't.discrete' into 't.cont'
#t.discrete = 1
#delta.t = 0.1 # Size of interval
#t.cont = t.discrete/delta.t # number of little steps
#lambda = 5 # events happen at rate = 10 per unit of time, which is delta.t 
#new.p = lambda*delta.t # probability is rate * single.time.interval
#pT.t.discrete(max.T = 10, p = 0.5, t.discrete = 10)
#pT.t.discrete(max.T = 10, p = new.p, t.discrete = t.discrete/delta.t)
#pT.t.cont(lambda = lambda, t.discrete = t.discrete, delta.t = 0.1)
#
### EXAMPLE:
##lambda.hour = 3 # 3 events per hour
##delta.t = 1/(24*7) # Size of time interval, in units of week
##t.discrete = 1 # units of week
#t.discrete/delta.t # number of intervals
#
#pT.t.cont(lambda = lambda, t.discrete = t.discrete, delta.t = delta.t)
#
######################################################
## rescaled binomial distribution
######################################################
#
#lambda = 3 # in units of events per hour
#t.discrete = 1 # in units of week
#delta.t = 1/(24*7) # Size of time interval, in units of week
#
#pk <- function(lambda, k, delta.t){
#	n.delta.t = 1/delta.t
#	term1 = choose(n.delta.t, k)
#	term2 = (lambda/n.delta.t)^k # k success
#	term3 = (1- (lambda/n.delta.t))^(n.delta.t-k) # n-k succes
#	pk = term1*term2*term3
#	print(pk)
#}
#
########################################################
## Some proofs
########################################################
#
#Eq.2.45 <- function(x){
#	a1 <- (1-x)
#	a2 <- (x^2)/factorial(2)
#	a3 <- (x^3)/factorial(3)
#	a4 <- (x^4)/factorial(4)
#	answer <- a1+a2-a3+a4
#	print(answer)
#}
#
## Check
#Eq.2.45(0.005)
#exp(-0.005)
#(1-0.005)
#
#Eq.2.48 <- function(lambda, delta.t, discrete.t){
#	p.fail.t <- (1-lambda*delta.t)^(t/delta.t)
#	return(p.fail.t)
#}
#
## CHECK:
#lambda.hour = 3 # 3 events per hour
#delta.t = 1/(24*7) # Size of time interval, in units of week
#t.discrete = 1 # units of week
#Eq.2.48(lambda = lambda.hour, delta.t = delta.t*0.00001, discrete.t =t.discrete)
#exp(-lambda*t.discrete)
#
######################################################
## Exponential distribution
######################################################
#
#fT.t <- function(lambda, discrete.t){
#	fT.t <- lambda*exp(-lambda*discrete.t)
#	print(fT.t)
#}
#
#lambda.hour = 3 # 3 events per hour
#delta.t = 1/(24*7) # Size of time interval, in units of week
#t.discrete = 1 # units of week
#lambda*Eq.2.48(lambda = lambda.hour, delta.t = delta.t*0.00001, discrete.t =t.discrete)
#fT.t(lambda = lambda, discrete.t = t.discrete)
#
#Exp.Dist <- function(max.T, lambda){
#	p.vec <- rep(0, max.T+1)
#	names(p.vec) <- 0:max.T
#	for (t in 0:max.T){
#		pT <- fT.t(lambda = lambda, discrete.t = t)
#		p.vec[t+1] <- pT
#	}
#	return(p.vec)
#}
#
#expected.X(Exp.Dist(lambda = 1, max.T = 10))
#
######################################################
## Poisson distribution
######################################################
#
#pk(lambda = 3, k = 10, delta.t = delta.t*0.0000001)
#dpois(x = 10, lambda = 3)
#
#