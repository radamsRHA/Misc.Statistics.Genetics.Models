##################################################
####Simulate mutations with skyline changes#######
##################################################

Coal.PopChange.Sim <- function(n, theta.list, l){
	print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	mut.count <- 0
	k <- n
	while (k > 1){
		theta <- theta.list[k]
		mut.prob <- (l*theta)/(k-1+l*theta)
		event <- sample(c("C", "M"), size = 1, prob = c(1-mut.prob, (mut.prob)))
		if (event == "C"){
			k = k - 1
		}
		if (event == "M"){
			mut.count = mut.count + 1
		}
	}
	return(mut.count)
}


#theta.list <- c(rep(0.005, 9), (0.005*2))
#CON.sim.fun <- replicate(100000, Coal.PopChange.Sim(n = 10, theta.list = theta.list, l = 80))
#hist(CON.sim.fun, breaks = 1000)
#mean(CON.sim.fun)



SimData <- function(sim.list, n.sample, outfile){
	print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	fun.file <- outfile
	for (locus in 1:length(sim.list)){
		for (sample in 1:n.sample){
			write(">sample", file = fun.file, append = T)
		}
		K = sim.list[locus]
		if (K == 0){
			write("//", file = fun.file, append = T)
		}
		if (K > 0){
			write(paste("//",toString(rep("*", as.numeric(K)))), file =  fun.file, append = T)
		}
	}
	close(file(outfile))
}


#SimData(sim.list = CON.sim.fun, n.sample = 10, outfile = "~/Desktop/fun.txt")

