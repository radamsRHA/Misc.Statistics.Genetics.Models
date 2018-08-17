
##################################################################################
#~~~~~~~ Kingman's coalescent, the function of coalescent times are i.i.d ~~~~~~~#
##################################################################################

Kingman1982 <- function(i){
	print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	E_Ti <- 2/(i*(i-1))
	Var_Ti <- E_Ti^2 
	fTi <- vector()
	for (ti in seq(0, (E_Ti+3*Var_Ti), 0.001)){
		fTi_ti <- choose(i, 2)*exp(-(choose(i, 2))*ti)
		fTi <- c(fTi, fTi_ti)
	}
	names(fTi) <- seq(0, (E_Ti+3*Var_Ti), 0.001)
	returnlist <- list(Expected_Ti = as.numeric(E_Ti), Variance_Ti =  as.numeric(Var_Ti), fTi_ti = fTi)
	return(returnlist)
}

### EXAMPLE, number of samples = 10
### x <- Kingman1982(4)
### plot(x[[3]], xaxt = "n", yaxt = "n", type = "l")
### axis(side = 1, at = seq(0, x[[1]]+3*x[[2]], 0.001)*1000, labels = seq(0, x[[1]]+3*x[[2]], 0.001))
### ETmrca with samples = 9: Kingman1982(2)[[1]]+Kingman1982(3)[[1]]+Kingman1982(4)[[1]]+Kingman1982(5)[[1]]+Kingman1982(6)[[1]]+Kingman1982(7)[[1]]+Kingman1982(8)[[1]]+Kingman1982(9)[[1]]

##############################################################################################################################
#~~~~~~~ Single generation transition probability, prob. of i lineages descend from j ancestors in previos generation ~~~~~~~#
##############################################################################################################################

Stirling2 <- function(n,m) 
{ 
	## Purpose:  Stirling Numbers of the 2-nd kind 
	## S^{(m)}_n = number of ways of partitioning a set of 
	##                      $n$ elements into $m$ non-empty subsets 
	## Author: Martin Maechler, Date:  May 28 1992, 23:42 
	## ---------------------------------------------------------------- 
	## Abramowitz/Stegun: 24,1,4 (p. 824-5 ; Table 24.4, p.835) 
	## Closed Form : p.824 "C." 
	## ---------------------------------------------------------------- 
	
	if (0 > m || m > n) stop("'m' must be in 0..n !") 
	k <- 0:m 
	sig <- rep(c(1,-1)*(-1)^m, length= m+1)# 1 for m=0; -1 1 (m=1) 
	## The following gives rounding errors for (25,5) : 
	## r <- sum( sig * k^n /(gamma(k+1)*gamma(m+1-k)) ) 
	ga <- gamma(k+1) 
	round(sum( sig * k^n /(ga * rev(ga)))) 
} 

descend.fact <- function(N, j){
	z <- vector()
	for (i in N:(N-j+1)){
		z <- c(z, i)
	}
	return(prod(z))
}

Gij <- function(i, j, N){
	Gij <- (Stirling2(i, j)*descend.fact(N, j))/(N^i)
	return(Gij)
}

### EXAMPLE, probability that 10 genetic lineages have 9 ancestors in the immediately previous generation for a population of N = 10
#Gij(10, 9, 10)

##################################################################################
#~~~~~~~ Tmrca and Ttotal.length functions ###############################~~~~~~~#
##################################################################################

ET.total <- function(n){
	print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	z <- vector()
	for (i in 1:(n-1)){
		x <- 1/i
		z <- c(z, x)
	}
	return(2*sum(z))
}

ET.mrca <- function(n){
	ET.mrca = 2*(1-1/(n))
	return(ET.mrca)
}

#ET.mrca(9)
