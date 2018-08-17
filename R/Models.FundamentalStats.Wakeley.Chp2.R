#####################################################
# Expectation of X (discrete)
#####################################################

expected.X <- function(PX){ # Expectation of X given PX
	E.X <- 0
	
	for (i in 1:length(PX)){
		xi = as.numeric(names(PX)[i])
		pxi = PX[i]
		
		E.X = E.X+xi*pxi
	}
	print(E.X)
}

## EXAMPLE:
#PX1 = c(0.04, 0.2, 0.3, 0.35, 0.11)
#names(PX1) = names(PX2) = 1:5
######################################################
#
######################################################
## Expectation of Y: a sum of RV variables
######################################################
#
#E.Y <- function(E.X.vec){ # Expectation of Y
#	E.y <- 0
#	
#	for (i in 1:length(E.X.vec)){
#		Xi = E.X.vec[i]
#		E.y = E.y + Xi
#	}
#	print(gsub(pattern = "XXX", replacement = E.y, x = "E[Y] = XXX"))
#}
#
## EXAMPLE: E[X.1] = 0, E[X.2] = 10, E[X.3] = 2.4, E[X.4] = 0
#E.Xi <- c(0,10,2.4, 0)
#names(E.Xi) <- c("E[X.1]", "E[X.2]", "E[X.3]", "E[X.4]")
#E.Y(E.X.vec = E.Xi)
#
## EXAMPLE 2: 
#PX1 = c(0.04, 0.2, 0.3, 0.35, 0.11)
#PX2 = c(0.2, 0.04, 0.11, 0.35, 0.3)
#names(PX1) = names(PX2) = 1:5
#
#E.Xi <- c(expected.X(PX = PX1), expected.X(PX = PX2))
#names(E.Xi) <- c("E[X.1]", "E[X.2]")
#E.Y(E.X.vec = E.Xi)
##########################################################
#
######################################################
## Obtaining the distribution of Y: a sum of RV variables, using a convolution (discrete case)
######################################################
#
#pY.y <- function(PX1, PX2, y){ # probability Y = y
#	x.max <- as.numeric(names(PX1)[length(PX1)])
#	p.y <- 0
#	
#	for (i in 1:x.max){
#		for (j in 1:x.max){
#			if (i + j == y){
#				X1.p = PX1[i]
#				X2.p = PX1[j]
#				
#				print(c(i,j))
#				p.y = p.y + (X1.p*X2.p)
#			}
#		}
#	}
#	print(p.y)
#}
#
## EXAMPLE:
#PX1 = c(0.04, 0.2, 0.3, 0.35, 0.11)
#PX2 = c(0.2, 0.04, 0.11, 0.35, 0.3)
#names(PX1) = names(PX2) = 1:5
#
#pY.y(PX1 = PX1, PX2 = PX2, y = 2)
#
######################################################
#
######################################################
## Expectation of Y: a sum of RV variables, and K is random as well
######################################################
#
#E.Y.K <- function(E.X.vec, E.K){ # Expectation of Y
#	E.y <- 0
#	
#	for (i in 1:length(E.X.vec)){
#		Xi = E.X.vec[i]
#		E.y = E.y + Xi
#	}
#	print(gsub(pattern = "XXX", replacement = E.K*E.y, x = "E[Y] = XXX"))
#}
#
## EXAMPLE:
#PX1 = c(0.04, 0.2, 0.3, 0.35, 0.11)
#PX2 = c(0.2, 0.04, 0.11, 0.35, 0.3)
#names(PX1) = names(PX2) = 1:5
#pK = c(0.5, 0.5)
#names(pK) = c(1,2)
#
#E.Xi <- c(expected.X(PX = PX1), expected.X(PX = PX2))
#names(E.Xi) <- c("E[X.1]", "E[X.2]")
#E.Y.K(E.X.vec = E.Xi, E.K = expected.X(PX = pK))
#