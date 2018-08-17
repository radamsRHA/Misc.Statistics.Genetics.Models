# allele freuency after migration (IV-1) with 2 populations
p1.prime <- function(m, p1, p2){
	p.prime <- (1-m)*p1 + m*p2
	return(p.prime)
	# p1.prime(m = 0.2, p1 = 1, p2 = 0)
}

# allele freuency after migration (IV-2), which 3 populations
p1.prime <- function(m12, m13, p1, p2, p3){
	p.prime <- (1-m12-m13)*p1 + m12*p2 + m13*p3
	return(p.prime)
	#p1.prime(m12 = 0.2, m13 = 0.2, p1 = 1, p2 = 0, p3 = 0)
}

# genotype frequncy (AA) in the newly mixed popualtion following migration
pij.m <- function(m, pij.1, pij.2){
	pij.prime <- (1-m)*pij.1 + m*pij.2
	return(pij.prime)
	#pij.m(m = 0.2, pij.1 = 1, pij.2 = 0)
}

# f(AA) in population 1 after migration, m are the weighted contributions of each population to the mixture
pAA <- function(m1, p1, m2, p2){
	pAA = m1*p1^2 + m2*p2^2
	return(pAA)
	#pAA(m1 = 0.8, p1 = 1, m2 = 0.2, p2 = 0)
}

# Estimating migration proportion
m.hat <- function(p, p1, p2){
	m = (p-p1)/(p2-p1)
	return(m)
	#m.hat(p = 0.129, p1 = 0.143, p2 = 0.05)
}

# one-island model # mainlan population freqiecies are not influenced by migration
one.island.model <- function(m, p0, pc){
	pt <- (1-m)*p0 + m*pc
	return(pt)
}

# island-model
island.model <- function(m, pi.0.vec){
	n = length(pi.0.vec) # number of islands
	p1.0 <- pi.0.vec[1]
	x = (1-m)*p1.0 # proportion of pop1 that does not migrate away
	z = 0
	
	for (j in 2:n){
		pj.0 <- pi.0.vec[j] # contribute to population 1 from other populations
		xx = pj.0* m/(n-1)
		z = z + xx
	}
	pi.t <- x +z
	return(pi.t)
	#island.model(m = 0.2, pi.0.vec = c(1,0, 0.5))
}

# Stepping-stone model
stepping.stone <- function(m, p1, p2, p3){
	p2.prime = (1-2*m/2)*p2 + m/2*p1 + m/2*p2
	return(p2.prime)
	#stepping.stone(m = 0.2, p1 = 1, p2 = 0.5, p3 = 0)
}

# general migration model
general.migration.model <- function(m12, m13, m21, m31, m23, m32, p1, p2, p3){
	M <- matrix(ncol = 3, nrow = 3)
	rownames(M) = colnames(M) = c("pop1", "pop2", "pop3")
	M[1,2:3] <- c(m12, m13)
	M[2,1] <- m21
	M[2,3] <- m23
	M[3,1:2] <- c(m31, m32)
	M[1,1] <- -sum(m12, m13)
	M[2,2] <- -sum(m21, m23)
	M[3,3] <- -sum(m31, m32)
	p.vec <- c(p1, p2, p3)
	names(p.vec) <- c("pop1", "pop2", "pop3")
	pt = M %*% as.vector(p.vec)
	print(pt)
	return(M)
	#general.migration.model(m12 = .25, m13 = .25, m21 = .25, m31 = .25, m23 = .25, m32 = .25, p1 = .5, p2 = .5, p3 = .5)
	#M[1,1]*0.5 + M[2, 1]*.5 + M[3,1] *.5
	# Above is when Ni * Mij = Nj *mji
}

# rate of one-island model convergence to continental frequencies
converge.rate <- function(m, p0, pc){
	r = (1-m)*(p0-pc)
	return(r)
	# converge.rate(m = 0.9, p0 = 1, pc = 0.001) convergence towards smaller allele
	# converge.rate(m = 0.01, p0 = 1, pc = 0.001) smaller rate
}

# haploid with selection
p.prime <- function(m, p, s){
	p.star <- function(p, s){
		p.star <- p/(1-(1-p)*s)
		return(p.star)
	}
	p.prime <- (1-m)*p.star(p = p,s = s) +m *0
	p.prime <- (p*(1-m))/(1-(1-p)*s)
	return(p.prime)
}

equalib <- function(p,m,s){
	delta.p <- (-m*p + p*(1-p)*s)/(1-(1-p)*s)
	mean.fitness = (1-(1-p)*s)
	numerator = p*(-m+(1-p)*s)
	
	print(c("delta.p", delta.p))
	print(c("mean.fitness", mean.fitness))
	print(c("numerator", numerator))
}

#equalib(p = 0, m = 0.2, s = 0.9)
