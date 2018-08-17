CoalSelectSim <- function(k, N, s){
print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	j = k
	sigma = 4*N*s
	coalescent <- list()
	while ( j > 1) {
		coal.rate <- (j*(j-1))/2
		branch.rate <- j*sigma/2
		event.rate <- coal.rate+branch.rate
		event.time <- rexp(n = 1, rate = event.rate)
		p.coal <- coal.rate/event.rate
		p.branch <- branch.rate/event.rate
		event <- sample(c("C", "B"), size = 1, replace = T, prob = c(p.coal, p.branch))
		if (event == "C"){
			j = j - 1
		}
		if (event == "B"){
			j = j + 1
		}
		print(c(event.time, event))
	}
}

#CoalSelectSim(4, 1000, .0001)
#sample(c("BD.1,AC.3", "BD.2"), size = 2, replace = T)
