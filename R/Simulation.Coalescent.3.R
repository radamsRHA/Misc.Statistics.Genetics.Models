#######################################
# Expectation of the total length of all branhes in the genealogy
#######################################

E.Total <- function(n){
	E = 0
	for (i in 2:n){
		E = E + i*(2/(i*(i-1)))
	}
	print(E)
}

#E.Total(n = 10000)

E.Tmrca <- function(n){
	E = 0
	for (i in 2:n){
		E = E + 2/(i*(i-1))
	}
	print(E)
}

#E.Tmrca(n = 100000)
#
#################################
#years.past <- 30000
#gen.rate <- 20
#N <- 3400
#generations.past <- years.past/gen
#ts <- generations.past/N
#

#######################################
# probability of k lineages ancestral to the sample at a time t in the past
#######################################

gnk.t <- function(n, k, t){
	t1 <- 1/choose(k,2)
	t2 <- 0
	for(i in k:n){
		t2i = (choose(i,2)*exp(-choose(i,2)*t))
		tx = 1
		for (j in k:n){
			if (j!=i){
				a = choose(j,2)/(choose(j,2)-choose(i,2))
				tx = tx*a
			}
		}
		t2 = t2+(t2i*tx)
	}
	gnk.1 <- t1*t2
	return(gnk.1)
}

gn1.t <- function(n, t){
	t2 <- 0
	for (i in 2:n){
		term1 = exp(-choose(i,2)*t)
		tx = 1
		for (j in 2:n){
			if (j!=i){
				tx = tx * choose(j,2)/(choose(j,2)-choose(i,2))
			}
		}
		t2 = term1*tx
	}
	return(t2)
}

fTnK <- function(n, t){
	z <- 0
	
	for (i in (k+1):n){
		term1 <- choose(i,2)*exp(-choose(i,2)*t)
		tx <- 1
		
		for (j in (k+1):n){
			if (j!=i){
				tx = tx * (choose(j,2)/(choose(j,2)-choose(i,2)))
			}
		}
		z <- z + term1*tx
	}
	return(z)
}

