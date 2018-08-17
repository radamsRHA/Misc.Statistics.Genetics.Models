#####################################################
# Derive binomial distribution from a bernoulli RV
#####################################################
library(gtools)
library(stringr)

Binom.Dist <- function(n, p){
	OUTCOME.PATTERNS <- permutations(n=2,r=n,v=c(1,0),repeats.allowed=T)
	DIST <- matrix(nrow = length(OUTCOME.PATTERNS[,1]), ncol = 1)
	y.DIST <- matrix(nrow = comb_with_replacement(n = 2, r = n), ncol = 1,data = 0)
	r.names <- vector()
	
	for (x in 1:length(OUTCOME.PATTERNS[,1])){
		pattern.p <- toString(OUTCOME.PATTERNS[x,1:n])
		number.success = str_count(pattern = "1", pattern.p )
		
		p.pattern = (p^number.success)*((1-p)^(n-number.success))
		r.names <- c(r.names, pattern.p)
		DIST[x,] <- p.pattern
	}
	rownames(DIST) <- r.names
	colnames(DIST) <- "p.outcome"
	for (i in 0:(length(y.DIST[,1])-1)){
		for (j in 1:length(rownames(DIST))){
			name.i <- rownames(DIST)[j]
			number.success = str_count(pattern = "1", name.i )
			if (number.success == i){
				y.DIST[i+1] = y.DIST[i+1]+DIST[j,]
			}
		}
		
	}
	rownames(y.DIST) = 0:(length(y.DIST[,1])-1)
	return(list(unfolded.dist <- DIST, pY = y.DIST))
}

## binomail coefficients satisfy the following recursion
Binom.Coeff <- function(n, y){
	bc <- choose(n-1, y) + choose(n-1, y-1)
	print(bc)
}

## Expectation of the binomial distribution
E.Y <- function(n,p){
	E.Y = 0
	for (i in 1:n){
		E.Y = E.Y + p
	}
	print(E.Y)
}

##EXAMPLE:
#plot(Binom.Dist(n = 10, p = 0.5)$pY)
#pX <- as.vector(Binom.Dist(n = 10, p = 0.5)$pY)
#names(pX) <- 0:10
#
#expected.X(PX = pX)
#E.Y(n = 10, p = 0.5)
######################################################

#####################################################
# Derive geometric distribution
#####################################################

Geom.Dist <- function(max.T, p){
	p.vec <- rep(0, max.T)
	names(p.vec) <- 1:max.T
	
	for (t in 1:max.T){
		pT = p*(1-p)^(t-1)
		p.vec[t] <- pT
	}
	return(p.vec)
}

#expected.X(PX = Geom.Dist(max.T = 100000, p = 0.4))
#E.T <- 1/0.4


# Proof of Equation 2.41

Eq.2.41 <- function(a, r){
	sum.to.1 <- 0

	for (k in 0:100000){
		sum.to.1 = sum.to.1 +(a*r^k)
	}
	print(sum.to.1)
	print(a/(1-r))
}

#Eq.2.41(a = 0.5, r = 0.5)
