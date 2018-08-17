#################################################
# GetSample data works similar to ParseAlleles: returns a list of 3:
# Alleles data (same as parse alleles)
# Mutation data: number of mutations for each locus
# Sample data: number of samples present at each locus
#################################################

GetSampleData <- function(allele.file){
print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	num.loci <- length(grep('//', readLines(allele.file)))
	infile = readLines(allele.file)
	locus.count = 1
	sample.count = 0
	alleles.data = vector(mode="list", length=num.loci)
	sample.data = vector()
	mut.data = vector()
	for (line in infile){
		sample.count = sample.count + 1
		if (str_count(line, "//") == 1){
			total.sample.count = sample.count
			k = str_count(line, '\\*') + str_count(line, '-')
			alleles.data[[locus.count]] <- c(toString(total.sample.count-1), toString(k))
			sample.data <- c(sample.data, total.sample.count)
			mut.data <- c(mut.data, k)
			locus.count = locus.count + 1
			sample.count = 0
		}
	}
	return(list(loci.dat = alleles.data, mutation.data = mut.data, sample.data = sample.data))
}


################################################
################ Infinite sites model 
################################################

Coal.sim <- function(n.list, theta){
print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	n = as.numeric(sample(n.list, size = 1))
	mut.vec <- vector()
	k = n
	mut.count = 0
	while (k > 1) {
		p.mut = (theta)/(k-1+(theta))
		p.coal = 1-p.mut
		event = sample(c("M", "C"), size = 1, prob = c(p.mut, p.coal))
		if (event == "C"){
			k = k-1
		}
		if (event == "M"){
			mut.count = mut.count + 1
			mut.vec[mut.count] = "M"
		}
	}
	return(length(mut.vec))
}

################################################
################ out put results of Coal.sim to a formatted file
################################################

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
			print(K)
		write(paste("//",toString(rep("*", as.numeric(K)))), file =  fun.file, append = T)
		}
	}
	close(file(outfile))
}


#SimData(sim.list = CON.sim.fun, n.sample = 3, outfile = '~/Desktop/fun.txt')
#################################################################################################
######## Repeat simulation for nreps to get a distribution of mutation simualted under the model
#################################################################################################
#con.1 <- theta.like.pyRAD('~/Desktop/ROUND1/CON_1.alleles', bound = 2, interval = 0.001)
#con.MLE.theta <- names(con.1[con.1 == max(con.1)])
#con.data <- GetSampleData(allele.file = '~/Desktop/ROUND1/CON_1.alleles')
#con.mut <- con.data[[2]]
#con.sample <- con.data[[3]]
#
#
#hist(con.mut)
#CON.sim.fun <- replicate(1000000, Coal.sim(n.list= con.sample, theta = as.numeric(names(con.MLE.theta))))
#hist(CON.sim.fun)
#

