#########
#########
#########


# ALGORITHM 1 (pg. 25)

sim.coal.1 <- function(n){
	print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	k = n
	coal.time.vec <- vector(mode="list", length=n)
	coal.sample <- list(1:n)
	while (k > 1){
		sim.time <- rexp(n = 1, rate = choose(k, 2))
		coal.pair <- sample(x = coal.sample[[1]], size = 2, replace = F)
		coal.pair <- qsub()
		
		k = k -1
	}
}


#sim.coal.1(10)
