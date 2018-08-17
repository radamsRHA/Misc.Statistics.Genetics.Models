

MCMC <- function(numeric.x, numeric.n, numeric.NumberOfIterations, numeric.mu, numeric.w){
	print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	
	########################
	# init vector for MCMC #
	########################
	vector.MCMC <- rep(NA, numeric.NumberOfIterations+1)
	vector.MCMC[1] <- 0.2
	
	############
	# run MCMC #
	############
	for (i in 1:numeric.NumberOfIterations){
		
		
		current.theta <- vector.MCMC[i]
		numeric.Random <- runif(n = 1, min = 0, max = 1)
		proposed.theta <- current.theta - (numeric.w/2) + (numeric.w*numeric.Random)
		if (proposed.theta < 0){
			proposed.theta = -proposed.theta
		}
		
		#######################
		# compute prior probs #
		#######################
		prior.current.theta <- (1/numeric.mu)*exp(-current.theta/numeric.mu)
		prior.proposed.theta <- (1/numeric.mu)*exp(-proposed.theta/numeric.mu)
		
		###############
		# compute LnL #
		###############
		LnL.current.theta <- (((3/4)-((3/4)*exp(-4*current.theta/3)))^numeric.x)*(((1/4)+((3/4)*exp(-4*current.theta/3)))^(numeric.n-numeric.x))
		LnL.proposed.theta <- (((3/4)-((3/4)*exp(-4*proposed.theta/3)))^numeric.x)*(((1/4)+((3/4)*exp(-4*proposed.theta/3)))^(numeric.n-numeric.x))
		
		#################
		# compute ratio #
		#################
		numerator <- prior.proposed.theta * LnL.proposed.theta
		denom <- prior.current.theta * LnL.current.theta
		
		alpha = numerator/denom
		
		numeric.U <- runif(n = 1, min = 0, max = 1)
		
		###############
		# check ratio #
		###############
		if (numeric.U < alpha){
			vector.MCMC[i+1] <- proposed.theta
		}
		if (numeric.U >= alpha){
			vector.MCMC[i+1] <- current.theta
		}
		
		
		
	}
	
	return(vector.MCMC)
}


#handle.MCMC <- MCMC(numeric.x = 90, numeric.n = 948, numeric.NumberOfIterations = 10^6, numeric.mu = 0.2, numeric.w = 0.01)
#hist(handle.MCMC[10000:1000000])


MCMC2 <- function(numeric.x, numeric.n, numeric.NumberOfIterations, numeric.mu, numeric.w){
	print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	
	########################
	# init vector for MCMC #
	########################
	vector.MCMC <- rep(NA, numeric.NumberOfIterations+1)
	vector.MCMC[1] <- 0.2
	
	############
	# run MCMC #
	############
	for (i in 1:numeric.NumberOfIterations){
		
		
		current.theta <- vector.MCMC[i]
		numeric.Random <- runif(n = 1, min = 0, max = 1)
		proposed.theta <- current.theta - (numeric.w/2) + (numeric.w*numeric.Random)
		if (proposed.theta < 0){
			proposed.theta = -proposed.theta
		}
		
		#######################
		# compute prior probs #
		#######################
		prior.current.theta <- log((1/numeric.mu)*exp(-current.theta/numeric.mu))
		prior.proposed.theta <- log((1/numeric.mu)*exp(-proposed.theta/numeric.mu))
		
		###############
		# compute LnL #
		###############
		LnL.current.theta <- log((((3/4)-((3/4)*exp(-4*current.theta/3)))^numeric.x)*(((1/4)+((3/4)*exp(-4*current.theta/3)))^(numeric.n-numeric.x)))
		LnL.proposed.theta <- log((((3/4)-((3/4)*exp(-4*proposed.theta/3)))^numeric.x)*(((1/4)+((3/4)*exp(-4*proposed.theta/3)))^(numeric.n-numeric.x)))
		
		#################
		# compute ratio #
		#################
		numerator <- prior.proposed.theta + LnL.proposed.theta
		denom <- prior.current.theta + LnL.current.theta
		
		alpha = exp(numerator-denom)
		
		numeric.U <- runif(n = 1, min = 0, max = 1)
		
		###############
		# check ratio #
		###############
		if (numeric.U < alpha){
			vector.MCMC[i+1] <- proposed.theta
		}
		if (numeric.U >= alpha){
			vector.MCMC[i+1] <- current.theta
		}
		
		
		
	}
	
	return(vector.MCMC)
}


#handle.MCMC <- MCMC2(numeric.x = 90, numeric.n = 948, numeric.NumberOfIterations = 10^6, numeric.mu = 0.2, numeric.w = 0.01)
#hist(handle.MCMC[10000:1000000])

