
PI <- function(pi.A, pi.C, pi.G, pi.T){
	PI <- matrix(0, nrow = 4, ncol = 4)
	PI[1,1] <- pi.A
	PI[2,2] <- pi.C
	PI[3,3] <- pi.G
	PI[4,4] <- pi.T
	rownames(PI) = colnames(PI) = c("A", "C", "G", "T")
	return(PI)
}

#PI(0.25, 0.25, 0.25, 0.25)
#R.matrix(r = .25)
## probability of "redunant mutation
#tr(PI(0.25, 0.25, 0.25, 0.25)%*%R.matrix(r = .25))
## probability not redundant
#1-tr(PI(0.25, 0.25, 0.25, 0.25)%*%R.matrix(r = .25))
## E[non redudant mutations|time = 1]
#u*(1-(tr(PI(0.25, 0.25, 0.25, 0.25)%*%R.matrix(r = .25))))
## if terms of Q matrix:
#u*-tr(PI(0.25, 0.25, 0.25, 0.25)%*%Q.matrix(r = 0.25))
## this is the mutation rate for the process
## under the JC69 model:
#u = 10^-9
#u*4*(.25*.75)
## to normalize the rates, we would set
#((.25*.75)+(.25*.75)+(.25*.75)+(.25*.75))^-1
#
#u = ((.25*.75)+(.25*.75)+(.25*.75)+(.25*.75))^-1
#t = 3/4
#expm(t*Q.matrix(r = 0.25)*u)
#
#u = 10^-9
#t = 10^9
#expm(u*t*Q.matrix(r = 0.25))
#