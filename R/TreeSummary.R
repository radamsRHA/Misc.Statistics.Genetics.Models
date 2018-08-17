############################################################################
#################### FTj = waiting time until next coal.event, to j -> j-1
############################################################################

fTj <- function(j, Tj.upperbound){
	print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	density <- vector()
	c = 0
	for (time.2N in seq(0, Tj.upperbound, 0.001)){
		c = c+ 1
		FTj <- choose(j, 2)*exp(-(choose(j, 2)*time.2N))
		density[c] <- FTj 
	}
	names(density) <- seq(0, Tj.upperbound, 0.001)
	return(density)
}


#### plot distribution of FT given j and uppderboun
#### plot(fTj(3, 2), type = "l")

############################################################################
#################### f(Tn...|G) function of the coalescent times given a genealogy
############################################################################

fTn_G <- function(n, upperbound.Tj){
	print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	final.density <- vector()
	for (time.2N in seq(0, upperbound.Tj, 0.001)){
		density <- vector()
		for (j in 2:n){
			fTj <-choose(j, 2)*exp(-(choose(j, 2)*time.2N))
			density <- c(density, fTj)
		}
		final.density <- c(final.density,(prod(density)))
	}	
	return(final.density)
}

# EXAMPLE: plot fTn | Gene tree = plot(fTn_G(10, .005), type = "l")

############################################################################
#################### f(G)*f(Tn|G) <- joint distribution of genealogy and coalescent times
############################################################################
fTnG <- function(n, upperbound.Tj){
	print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	final.dens <- vector()
	for (time.2N in seq(0, upperbound.Tj, 0.00001)){
		density <- vector()
		for (j in 2:n){
			f <- exp(-(choose(j, 2)*time.2N))
			density <- c(density, f)
		}
	final.dens <- c(final.dens, prod(density))
	}
	return(final.dens)
}

#plot(fTnG(40, .01), type = "l")
# Example probability of geneaogy and coalescent times over Tj = fTnG(4, .1)

############################################################################
#################### ETmrca is the sum of Expected coalescent times
############################################################################
