print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")

pi.matrix <- function(pi.list){
	pi.A <- pi.list["A"]; pi.C <- pi.list["C"]; pi.G <- pi.list["G"]; pi.T <- pi.list["T"];
	pi.mat <- matrix(ncol = 4, nrow = 4)
	pi.mat[1,] <- c(pi.A, pi.C, pi.G, pi.T)
	pi.mat[2,] <- c(pi.A, pi.C, pi.G, pi.T)
	pi.mat[3,] <- c(pi.A, pi.C, pi.G, pi.T)
	pi.mat[4,] <- c(pi.A, pi.C, pi.G, pi.T)
	rownames(pi.mat) <- c("A", "C", "G", "T")
	colnames(pi.mat) <- c("A", "C", "G", "T")
	return(pi.mat)
}

R.matrix <- function(relative.rates.list){
	R <- matrix(nrow = 4, ncol = 4)
	a = relative.rates.list["a"]; b = relative.rates.list["b"]; c = relative.rates.list["c"]; d = relative.rates.list["d"]; 
	e = relative.rates.list["e"]; f = relative.rates.list["f"];
	R[1,] <- c(0, a, b, c)
	R[2,] <- c(a, 0, d, e)
	R[3,] <- c(b, d, 0, f)
	R[4,] <- c(c, e, f, 0)
	rownames(R) <- c("A", "C", "G", "T")
	colnames(R) <- c("A", "C", "G", "T")
	return(R)
}

Q.matrix <- function(u){
	Q <- matrix(ncol = 4, nrow = 4)
	Q[1,] <- c(-u, u, u, u)
	Q[2,] <- c(u, -u, u, u)
	Q[3,] <- c(u, u, -u, u)
	Q[4,] <- c(u, u, u, -u)
	rownames(Q) <- c("A", "C", "G", "T")
	colnames(Q) <- c("A", "C", "G", "T")
	return(Q)
}

#rate.list <- c(1, 1, 1, 1, 1, 1)
#names(rate.list) <- c("a", "b", "c", "d", "e", "f")
#pi.list <- c(.25, .25, .25 ,.25)
#names(pi.list) <- c("A", "C", "G", "T")
#
#total.rate = 2.2*10^-9 # 3*lambda
#lambda = total.rate/3
#u = lambda*4
#t = 10^6
#P0+(R.matrix(rate.list)*pi.matrix(pi.list)*u*t) ### Same as below
#P0+(R.matrix(rate.list)*pi.matrix(pi.list)*total.rate/3*4*t) ## different parameterizations
#





######################### Compare parameterizations of Q

Q.yang <- function(u){
	lambda = u/4
	Q <- matrix(nrow = 4, ncol = 4)
	Q[1,] <- c(-3*lambda, lambda, lambda, lambda)
	Q[2,] <- c(lambda, -3*lambda, lambda, lambda)
	Q[3,] <- c(lambda, lambda, -3*lambda, lambda)
	Q[4,] <- c(lambda, lambda, lambda, -3*lambda)
	rownames(Q) <- c("A", "C", "G", "T")
	colnames(Q) <- c("A", "C", "G", "T")
	return(Q)
}









Q.hb <- function(u){
	Q <- matrix(nrow = 4, ncol = 4)
	Q[1,] <- c(-3*u*.25, u*.25, u*.25, u*.25)
	Q[2,] <- c(u*.25, -3*u*.25, u*.25, u*.25)
	Q[3,] <- c(u*.25, u*.25, -3*u*.25, u*.25)
	Q[4,] <- c(u*.25, u*.25, u*.25, -3*u*.25)
	rownames(Q) <- c("A", "C", "G", "T")
	colnames(Q) <- c("A", "C", "G", "T")
	return(Q)
}

#t = 10^6
#total.rate = 2.2*10^-9 # 3*lambda, 3*u/4 total leaving a state
#lambda = total.rate/3 # instant.rate from one nt to another
#u = lambda*4 # mean.instant.sub.rate
#Q.yang(u)
#Q.hb(u)
#
#total.rate = -(.25*Q.hb(u)[1,1])*4 # can derive the total rate from the Q and pi matrix (sum freq.i*Qii)
#
#R.matrix <- function(relative.rates.list){
#	R <- matrix(nrow = 4, ncol = 4)
#	a = relative.rates.list["a"]; b = relative.rates.list["b"]; c = relative.rates.list["c"]; d = relative.rates.list["d"]; 
#	e = relative.rates.list["e"]; f = relative.rates.list["f"];
#	R[1,] <- c(0, a, b, c)
#	R[2,] <- c(a, 0, d, e)
#	R[3,] <- c(b, d, 0, f)
#	R[4,] <- c(c, e, f, 0)
#	rownames(R) <- c("A", "C", "G", "T")
#	colnames(R) <- c("A", "C", "G", "T")
#	return(R)
#}
#
#rate.list <- c(1, 1, 1, 1, 1, 1)
#names(rate.list) <- c("a", "b", "c", "d", "e", "f")
#R.matrix(rate.list)
#pi <- c(.25, .25, .25 ,.25)
#