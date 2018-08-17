# The number of descendents of a particular gene i in generation t is a stochastic variable
# its distribution is easily computed: each time a new generation is created at time (t+1), it has the probability of picking a parent = 1/2N
# this sampling is repeated 2N times with replacement

NumDescendants <- function(N, k){
	# p(vi = k) = k is binomial distribution with parameters m = 2N, p = 1/2N
	pvi.k = choose((2*N), k)*(1-1/(2*N))^((2*N)-k)*(1/(2*N))^k
	pvi.k = dbinom(x = k, size = (2*N), prob = 1/(2*N))
	
	out.string1 <- gsub(pattern = 'XXX', replacement = k, x = 'Probablity of one particular parent having XXXX offspring in a population of size YYY diploid individuals (WWW chromosomes): ZZZ')
	out.string2 <- gsub(pattern = "YYY", replacement = N, x = out.string1)
	out.string3 <- gsub(pattern = "ZZZ", replacement = pvi.k, x = out.string2)
	out.string4 <- gsub(pattern = "WWW", replacement = (2*N), x = out.string3)
	print(out.string4)

	out.string5 <- gsub(pattern = 'XXX', replacement = (1-(1/(2*N))), x = 'the variance of vi in a population of size YYY diploid individuals (WWW chromosomes): XXX, and the mean = 1')
	out.string6 <- gsub(pattern = "YYY", replacement = N, x = out.string5)
	out.string7 <- gsub(pattern = "WWW", replacement = (2*N), x = out.string6)
	print(out.string7)
	return(pvi.k)
}

#NumDescendants(N = 1000000, k = 10)

# If 2N is large, than vi is almost poisson distributed as ~p(1)

NumDescendants.LargeN <- function(k){
	pvi.k = 1/(factorial(k))*exp(-1)
	pvi.k = (dpois(x = k,lambda = 1))
	#print(pvi.k)
	return(pvi.k)
}


## Compared
#NumDescendants(N = 10000000, k = 10)
#NumDescendants.LargeN(k = 10)
#
### Probability that a gene does not leave any descendents
#NumDescendants(N = 10000000, k = 0)
#NumDescendants.LargeN(k = 0)
#
### This is also the fraction of genes that do not leave any descendants
### the fractio of genest that leave at least one descendent is:
#1-NumDescendants.LargeN(k = 0)
#
### 0.63 of all genes leave at least one descendant in the next generation
### Thus, the present day population descendents from a relatively small fractio of genes a few generations ago, names 0.63^t
AncestralOriginate <- function(N, N.prime){
	# number of generations to get N.prime ancestral lineages
	x <- log(N.prime/N)/log(0.63)
	out.string1 <- gsub(pattern = 'XXX', replacement = N, x = "the current population os size XXX descended from YYY genes, ZZZ geneations ago")
	out.string2 <- gsub(pattern = 'YYY', replacement = N.prime, x = out.string1)
	out.string3 <- gsub(pattern = 'ZZZ', replacement = x, x = out.string2)
	print(out.string3)
}


