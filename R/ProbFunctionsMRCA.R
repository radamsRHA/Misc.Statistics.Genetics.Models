##################################################################################
######## Probability functions of Tmrca ##########################################
##################################################################################

descend.fact <- function(n, i){
	print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	ni <- vector()
	for (x in n:(n-i+1)){
		ni <- c(ni, x)
	}
	return(prod(ni))
}

ascend.fact <- function(n, i){
	print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	ni <- vector()
	for (x in n:(n+i-1)){
		ni <- c(ni, x)
	}
	return(prod(ni))
}


Tavare.1984 <- function(n, t){
	print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	z <- vector()
	for (i in 2:n) {
		t1 = ((2*i-1))*(-1)^i
		t2 = descend.fact(n, i)
		t3 = ascend.fact(n, i)
		t4 = choose(i, 2)
		t5 = exp(-(choose(i, 2))*t)
		x <- ((t1*t2)/t3)*t4*t5
		z <- c(z, x)
		
	}
	return(sum(z))
}

#z <- vector()
#for (t in seq(0, 4, 0.001)){
#	x <- Tavare.1984(10, t)
#	z <- c(z, x)
#}

#plot(z)
