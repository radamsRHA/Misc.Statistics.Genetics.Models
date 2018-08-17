library(stringr)

#################################################
# Tavare 1984, probability of k mutations|theta\n
#################################################

pSk <- function(n, k, theta){
print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	z <- rep(NA,(n-1))
	for (i in 2:n){
		t1 <- (-1)^i
		t2 <- choose(n-1, i-1) # combinations of coalescent histories
		t3 <- (i-1)/(theta+i-1) # probability of coalescent
		t4 <- (theta/(theta+i-1))^k # probability of mutations k
		pSk <- prod(t1, t2, t3, t4)
		z[i-1] = pSk
	}
	pSk <- z
	return(sum(pSk))
}

#############################################################################
# function to parse out allele information from pyRAD (n and k for each locus)
#############################################################################


ParseAlleles <- function(allele.file){
print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	num.loci <- length(grep('//', readLines(allele.file)))
	infile = readLines(allele.file)
	locus.count = 1
	sample.count = 0
	alleles.data = vector(mode="list", length=num.loci)
	for (line in infile){
		sample.count = sample.count + 1
		if (str_count(line, "//") == 1){
			total.sample.count = sample.count
			k = str_count(line, '\\*') + str_count(line, '-')
			alleles.data[[locus.count]] <- c(toString(total.sample.count-1), toString(k))
			locus.count = locus.count + 1
			sample.count = 0
		}
	}
	return(alleles.data)
}

#########################################################################################
# Estimate MLE of theta using Tavare 1984 and grid approximation, set bounrd and intervals
# Returns list of log likelihoods and corresponding theta values
#########################################################################################


theta.like.pyRAD <- function(alleles.data, bound, interval){
print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	ln <- rep(NA,bound/interval)
	theta.count = 0
	for (theta in seq(0, bound, interval)){
		theta.count = theta.count + 1
		locus.count = 0
		theta.v <- rep(NA,length(alleles.data))
		for (locus in alleles.data){
			locus.count = locus.count + 1
			n = as.numeric(locus[1])
			k = as.numeric(locus[2])
			pk <- log(pSk(n, k, theta))
			theta.v[locus.count] <- pk
		}
		ln[theta.count] <- sum(theta.v)
	}
	names(ln) <- seq(0, bound, interval)
	return(ln)
}


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

################################################################################################################
################################################################################################################
################################################################################################################

#con.1 <- theta.like.pyRAD('~/Desktop/ROUND1/CON_1.alleles', bound = 2, interval = 0.001)
#hell.1 <- theta.like.pyRAD('~/Desktop/ROUND1/HELL_1.alleles', bound = 2, interval = 0.001)
#lut.1 <- theta.like.pyRAD('~/Desktop/ROUND1/LUT_1.alleles', bound = 2, interval = 0.001)
#nun.1 <- theta.like.pyRAD('~/Desktop/ROUND1/NUN_1.alleles', bound = 2, interval = 0.001)
#ore.1 <- theta.like.pyRAD('~/Desktop/ROUND1/ORE_1.alleles', bound = 2, interval = 0.001)
#vir.1 <- theta.like.pyRAD('~/Desktop/ROUND1/VIR_1.alleles', bound = 2, interval = 0.001)
#cerb.1 <- theta.like.pyRAD('~/Desktop/ROUND1/CON_1.alleles', bound = 2, interval = 0.001)
#
