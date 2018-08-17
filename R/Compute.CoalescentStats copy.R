sim.gene <- function(n){
	i = n
	results <- rep(NA, (n-1))
	c = 0
	while (i > 1){
		r.lambda <- choose(i, 2)
		r.event.time <- rexp(n = 1, rate = r.lambda)
		c = c + 1
		results[c] <-r.event.time
		i = i-1
	}
	return(results)
}

E.Total <- function(n){
	a <- 1:(n-1)
	b <- 2*sum(1/a)
	return(b)
}

E.Tmrca <- function(n){
	b <- 2*(1-1/n)
	return(b)
}

## Average time to common ancestor, n = 10
#E.Tmrca(n = 10)
#mean(replicate(n = 10000, sum(sim.gene(n = 10))))
#
## Average total length of tree
#E.Total(n = 3)
#mean(replicate(n = 10000, sum((3:2)*sim.gene(n = 3))))
#
#E.Total(n = 10)
#mean(replicate(n = 10000, sum((10:2)*sim.gene(n = 10))))
#