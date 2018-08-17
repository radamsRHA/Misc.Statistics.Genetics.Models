Compute.LnL <- function(vector.ObservedGenotypes, vector.Populationfrequencies){
	print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	
	numeric.LnL <- 0
	
	for (i in 1:length(vector.ObservedGenotypes)){
		
		numeric.Genotype <- vector.ObservedGenotypes[i]
		numeric.PopFreq <- vector.Populationfrequencies[i]
		numeric.LnL.locus <- dbinom(x = numeric.Genotype, size = 2, prob = numeric.PopFreq, log = T)
		numeric.LnL <- numeric.LnL + numeric.LnL.locus
	}
	return(numeric.LnL.locus)
}


Bayesian.Structure <- function(numeric.NumberOfIterations, vector.ObservedGenotypes, vector.Pop1, vector.Pop2, vector.Pop3, vector.Pop4){
	print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	
	############################
	# Set MCMC starting values #
	############################
	matrix.MCMC.Z <- rep(NA, numeric.NumberOfIterations + 1)
	matrix.MCMC.Z[1] <- 1
	
	matrix.P <- matrix(nrow = 42, ncol = length(vector.Pop2))
	matrix.P[1,] <- vector.Pop1
	matrix.P[2,] <- vector.Pop2
	matrix.P[3,] <- vector.Pop3
	matrix.P[4,] <- vector.Pop4
	
	vector.prior <- c(1/4, 1/4, 1/4, 1/4)
	#vector.prior <- c((1-0.8)/3, (1-0.8)/3, (1-0.8)/3, 0.8)
	vector.LNL <- rep(NA, numeric.NumberOfIterations + 1)

	##########################
	# Run the MCMC algorithm #
	##########################
	for (i in 1:numeric.NumberOfIterations){
		
		############################
		# Get current param values #
		############################
		numeric.Current.Z <- matrix.MCMC.Z[i]
		
		######################
		# Propose new params #
		######################
		numeric.Propsed.Z <- sample(x = 1:4, size = 1)
		
		###############
		# compute LnL #
		###############
		numeric.LnL.Current.Z <- Compute.LnL(vector.ObservedGenotypes = vector.ObservedGenotypes,vector.Populationfrequencies = matrix.P[numeric.Current.Z,])
		numeric.LnL.Proposed.Z <- Compute.LnL(vector.ObservedGenotypes = vector.ObservedGenotypes,vector.Populationfrequencies = matrix.P[numeric.Propsed.Z,])
		
		###############
		# check ratio #
		###############
		numeric.Current.Prior <- vector.prior[numeric.Current.Z]
		numeric.Proposed.Prior <- vector.prior[numeric.Propsed.Z]
		numerator <- log(numeric.Proposed.Prior)+numeric.LnL.Proposed.Z
		denom <- log(numeric.Current.Prior)+numeric.LnL.Current.Z
		alpha <- exp(numerator - denom)
		numeric.U <- runif(n = 1, min = 0, max = 1)
		
		###############
		# check ratio #
		###############
		if (numeric.U < alpha){
			matrix.MCMC.Z[i+1] <- numeric.Propsed.Z
		}
		if (numeric.U >= alpha){
			matrix.MCMC.Z[i+1] <- numeric.Current.Z
		}
		
		
		
		vector.LNL[i] <- numeric.LnL.Current.Z
		
	}
	
	return(list(vector.LNL = vector.LNL, matrix.MCMC.Z = matrix.MCMC.Z))
}






# generate vector of population allele frequencies
#vector.Pop1AlleleFreqs <- runif(n = 10^3, min = 0, max = 1)
#vector.Pop1AlleleFreqs <- rep(0.7, 10^3)
#vector.Pop2AlleleFreqs <- rep(1/50, 10^3)
#vector.Pop3AlleleFreqs <- rep(1/100, 10^3)
#vector.Pop4AlleleFreqs <- rep(1/200, 10^3)
#vector.ObservedGenotypes <- rbinom(n = 10^3, size = 2, prob = vector.Pop1AlleleFreqs)

#vector.MCMC <- Bayesian.Structure(numeric.NumberOfIterations = 10^3, 
#																	vector.ObservedGenotypes = vector.ObservedGenotypes, 
#																	vector.Pop1 = vector.Pop1AlleleFreqs, 
#																	vector.Pop2 = vector.Pop2AlleleFreqs,
#																	vector.Pop3 = vector.Pop3AlleleFreqs,
#																	vector.Pop4 = vector.Pop4AlleleFreqs)
#
#
#
#
#hist(vector.MCMC$matrix.MCMC.Z)
##plot(vector.MCMC$vector.LNL, type = "l")
#