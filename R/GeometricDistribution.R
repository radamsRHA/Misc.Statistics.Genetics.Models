# Probability of a specific next step in discrete or continous process only depends on the current state
# if time is discrete, we use the geometric distribution (continuous = exponential)
# probability of the first event occurring on the jth step:
GeometricP <- function(p, j){
	pT.j = p*(1-p)^(j-1)
	print(pT.j)
}

# probability 