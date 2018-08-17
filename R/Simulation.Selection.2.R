SinglePopCoal.Select <- function(n, N, s){
print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	event.times <- vector()
	sample.list <- 1:n
	coalescent.history <- vector()
	k = n
	while (k > 1){# Cycle through sample coalescent events
		coal.rate <- (k*(k-1)/2) # Exponential rate parameter
		branch.rate <- ((4*N*s)/2)*k
		event.rate <- coal.rate + branch.rate
		random.event.time <- rexp(n = 1, rate = event.rate ) # Random time sampled from exponential distribution
		event.times <- c(event.times, random.event.time)
		
		p.coal.event <-coal.rate/event.rate
		p.branch.event <- branch.rate/event.rate
		sim.event <- sample(c("COAL", "BRANCH"), size = 1, replace = T, prob = c(p.coal.event, p.branch.event))
		coalescent.history <- c(coalescent.history, sim.event)
		
		if (sim.event == "COAL"){
			k = k - 1
		}
		if (sim.event == "BRANCH"){
			k = k + 1
		}
	}
	print(cbind(coalescent.history, event.times))
}


#y <- SinglePopCoal.Select(10, 10000, .0001)
