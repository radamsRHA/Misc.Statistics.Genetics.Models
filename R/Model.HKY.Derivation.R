## HKY Model Ph. 189 Nielson
#
###########
##  Specify HKY model with k = 5 and parameters below
###########
#k = 5
#pi.A <- 0.4
#pi.C <- 0.3
#pi.G <- 0.2
#pi.T <- 0.1
#
#Q <- matrix(nrow = 4, ncol = 4)
#Q[1,1] <- -sum(pi.C, pi.G*5, pi.T)
#Q[1,2:4] <- c(pi.C, pi.G*5, pi.T)
#Q[2,] <- c(pi.A, 0, pi.G, 5*pi.T)
#Q[2,2] <- -sum(pi.A, 0, pi.G, 5*pi.T)
#Q[3,] <- c(k*pi.A, pi.C, 0, pi.T)
#Q[3,3] <- -sum(k*pi.A, pi.C, 0, pi.T)
#Q[4,] <- c(pi.A, k*pi.C, pi.G, 0)
#Q[4,4] <- -sum(pi.A, k*pi.C, pi.G, 0)
#
#mu <- -1/(sum(pi.A*Q[1,1], pi.C*Q[2,2], pi.G*Q[3,3], pi.T*Q[4,4]))
#
#Q.matrix <- round(x = Q*mu, digits = 3)
#rownames(Q.matrix) <- colnames(Q.matrix) <- c("A", "C", "G", "T")
#
###########
##  Interpration of changes on a tree
###########
#branch.length <- 0.5
#ancestor <- "G"
#
## simulate evolution starting with ancestral "G"
## the only relevant row of the Q matrix is the third one
#relevant.model <- Q.matrix[3,]
#
#
## Overall rate of change away rom G is simply -qGC = 1.519
#overall.rate.away.G <- -relevant.model["G"]
#lambda <- overall.rate.away.G
#
## waiting time between events is exponential distributed
## generate random waiting time using the following compuation:
#random.u <- runif(n = 1, min = 0, max = 1)
#t <- -1/overall.rate.away.G * log(random.u) # this is the next time until a substitution occurs!
#
## rate matrix also specifiyes the probabilitys of substitution away from G
#pGA <- relevant.model["A"]/-relevant.model["G"]
#pGC <- relevant.model["C"]/-relevant.model["G"]
#pGT <- relevant.model["T"]/-relevant.model["G"]
#
## equal to prob
#sample.decendent <- sample(x = c("A", "C", "T"), size = 10000, prob = c(pGA, pGC, pGT), replace = T)
#table(sample.decendent)/sum(table(sample.decendent))
#
#Q.matrix[3,][rownames(Q.matrix)!="G"]
#
#
#results <- vector()
#
#time <- 0
#current.state <- "G"
#
#while (time <= 0.5){
#	random.u <- runif(n = 1, min = 0, max = 1)
#	t <- -1/overall.rate.away.G * log(random.u)
#	time <- time + t
#		
#		if (time < 0.5){
#		
#		relevant.model <- Q.matrix[current.state,]
#		other.states <- relevant.model[names(relevant.model)!=current.state]
#		p.other.states <- other.states/-Q.matrix[current.state, current.state]
#		sample.decendent <- sample(x = names(p.other.states), size = 1, prob = p.other.states, replace = T)
#		current.state <- sample.decendent
#		
#	} else {current.state = current.state}
#	
#}
#
#results <- c(results, current.state)
#
#table(results)
#sum(table(results))
#
#
##### 
## we can caluclate the transition probability exactly
#library(expm)
#P <- expm(Q.matrix*branch.length)
#