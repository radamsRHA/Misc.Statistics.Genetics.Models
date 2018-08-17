BS <- function(nd, n, B){
	d.hat <- function(p.hat){
		d.hat <- -log(1-p.hat)
		return(d.hat)
	}
	
	delta.d.hat <- function(d.hat){
		delta <- (d.hat-d.hat.sample.bar)^2
		return(delta)
	}
	
	bs.sample <- (rbinom(n = B, size = n, prob = nd/n))/n
	
	d.hat.sample <- sapply(bs.sample, d.hat)
	d.hat.sample.bar <- mean(d.hat.sample)
	
	
	delta.d <- sapply(d.hat.sample, delta.d.hat)
	summation.delta.d <- sum(delta.d)
	
	VarB.dhat <- 1/(B-1)*summation.delta.d
	print(VarB.dhat)
}


#sqrt(BS(nd = 17, n = 140, B = 1000))
