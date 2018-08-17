MultiCoal.2species <- function(divergence.time, nA, nB){
print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	event.time <- vector()
	event.time <- c(event.time, 0)
	sample.list.A <- 1:nA
	sample.list.B <- 1:nB
	coalescent.topology.A <- vector()
	coalescent.topology.A <- c(coalescent.topology.A, "start")
	coalescent.topology.B <- vector()
	coalescent.topology.B <- c(coalescent.topology.B, "start")
	kA <- nA
	kB <- nB
	k = kA + kB
	while (tail(event.time, n = 1) < divergence.time){
		if (kA > 1){
			rate.coal.A <- choose(kA, 2)
			time.coal.A <- rexp(n = 1, rate = rate.coal.A) + tail(event.time, n = 1)
		}
		if (kB > 1){
			rate.coal.B <- choose(kB, 2)
			time.coal.B <- rexp(n = 1, rate = rate.coal.B) + tail(event.time, n = 1)
		}
		if (kA == 1){
			time.coal.A = "END"
		}
		if (kB == 1){
			time.coal.B = "END"
		}
		if (min(time.coal.A, time.coal.B) == "END"){
			event.time <- c(event.time, divergence.time)
		}
		if (min(time.coal.A, time.coal.B) != "END"){
			if (min(time.coal.A, time.coal.B) < divergence.time){
				if (time.coal.A < time.coal.B){
					event.time <- c(event.time, time.coal.A)
					coal.pair <- sample(sample.list.A, size = 2, replace = F)
					coalescent.topology.A <- c(coalescent.topology.A, toString(coal.pair))
					sample.list.A <- sample.list.A[sample.list.A != coal.pair[1]]
					sample.list.A <- sample.list.A[sample.list.A != coal.pair[2]]
					pair <- toString(coal.pair)
					pair <- gsub("XX", pair, "(XX)")
					sample.list.A <- c(sample.list.A, (pair))
					kA = kA -1
				}
				if (time.coal.B < time.coal.A){
					event.time <- c(event.time, time.coal.B)
					coal.pair <- sample(sample.list.B, size = 2, replace = F)
					coalescent.topology.B <- c(coalescent.topology.B, toString(coal.pair))
					sample.list.B <- sample.list.B[sample.list.B != coal.pair[1]]
					sample.list.B <- sample.list.B[sample.list.B != coal.pair[2]]
					pair <- toString(coal.pair)
					pair <- gsub("XX", pair, "(XX)")
					sample.list.B <- c(sample.list.B, (pair))
					kB = kB - 1
				}
			}
			if (min(time.coal.A, time.coal.B) > divergence.time){
				event.time <- c(event.time, divergence.time)
			}
		}
	}
	print(tail(event.time, n = 1))
	return(list(sample.list.A, sample.list.B))
}

#ABC <- MultiCoal.2species(1, 10, 10)
#end = c(ABC[[1]], ABC[[2]])
#end
