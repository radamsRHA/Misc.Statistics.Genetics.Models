library(phytools)
SinglePopCoal.Yang2 <- function(n, samples){
print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")

	event.times <- vector()
	sample.list <- samples
	coalescent.history <- vector()
	k = n
	while (k > 1){# Cycle through sample coalescent events
		event.time.rate <- (k*(k-1)/2) # Exponential rate parameter
		random.event.time <- rexp(n = 1, rate = event.time.rate) # Random time sampled from exponential distribution
		event.times <- c(event.times, random.event.time)
		k = k - 1
		coal.pair <- sample(sample.list, size = 2, replace = F)
		coalescent.history <- c(coalescent.history, toString(coal.pair))
		sample.list <- sample.list[sample.list != coal.pair[1]]
		sample.list <- sample.list[sample.list != coal.pair[2]]
		pair <- toString(coal.pair)
		pair <- gsub("XX", pair, "(XX)")
		sample.list <- c(sample.list, (pair))
	}
	coalescent <- paste(coalescent.history[n-1], ";")
	coalescent <- paste("(", coalescent)
	coalescent <- gsub(";", ");", coalescent)
	return(coalescent)
}

#y <- read.tree(text =SinglePopCoal.Yang2(length(end), end))
#sub.want = ABC[[1]][3]
#sub.want = paste(sub.want, ";")
#sub.want = read.tree(text = sub.want)
#
#sub.want.names = GetSubtreeNode(y, sub.want)
#
#tree<-paintSubTree(y,sub.want.names ,state="2",stem=T)
#cols<-c("blue","red","green"); names(cols)<-c(1,2,3)
#plotSimmap(tree,cols,lwd=3,pts=F)
#