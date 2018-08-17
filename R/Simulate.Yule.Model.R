p1x <- function(p, lambda, u, x){
	num <- (p*(lambda-u)^2)*exp(-((lambda-u)*x))
	denom.1 <- (p*lambda+(lambda*(1-p)-u)*exp(-(lambda-u)*x))^2
	p1x <- num/denom.1
	return(p1x)
}

#
#z <- vector()
#for (i in seq(0, .05, .0001)){
#	x <- p1x(1, 40, 0, i)
#	z <- c(z, x)
#}
#
##plot(z, type = "l")
#