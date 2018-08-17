#####################################################################
## Poisson model for the number of offspring in the next generation #
#####################################################################
#nt <- 1
#birth.rate = 0.011
#mean.TimeUntilDivision = 1/birth.rate
#
#Random.WaitTime.UntilEvent <- rexp(n = 1, rate = nt*birth.rate)
#
##########################
## Model next generation #
##########################
#nt <- 2
#Random.WaitTime.UntilEvent <- rexp(n = 1, rate = nt*birth.rate)
#
#############################################
## Simulate pure birth model (uniform rate) #
#############################################
#Simulate.PureBirthModel <- function(n0, birth.rate, numeric.StopTime){
#	vector.PopulationSize.Over.Time <- vector()
#	vector.EvenTimes <- vector()
#	vector.PopulationSize.Over.Time[1] <- n0
#	vector.EvenTimes[1] <- 0
#	numeric.CurrentTime <- 0
#	numeric.CurrentPopulationSize <- n0
#	numeric.EventCounter <- 1
#	
#	while(numeric.CurrentTime <= numeric.StopTime){
#		numeric.Random.WaitingTime <-  rexp(n = 1, rate = numeric.CurrentPopulationSize*birth.rate)
#		numeric.CurrentTime <- numeric.CurrentTime + numeric.Random.WaitingTime
#		numeric.CurrentPopulationSize <- numeric.CurrentPopulationSize + 1
#		numeric.EventCounter <- numeric.EventCounter + 1
#		vector.PopulationSize.Over.Time[numeric.EventCounter] <- numeric.CurrentPopulationSize
#		vector.EvenTimes[numeric.EventCounter] <- numeric.CurrentTime
#	}
#	names(vector.PopulationSize.Over.Time) <- vector.EvenTimes
#	return(vector.PopulationSize.Over.Time)
#}
##
##RESULTS <- Simulate.PureBirthModel(n0 = 10, birth.rate = 0.011, numeric.StopTime = 200)
##plot(x = names(RESULTS), y = RESULTS, type= "l")
##
##############################################
## Simulate birth-death model (uniform rate) #
##############################################
#Simuulate.Birth.Death.Model <- function(n0, birth.rate, death.rate, numeric.StopTime){
#	vector.PopulationSize.Over.Time <- vector()
#	vector.EvenTimes <- vector()
#	vector.PopulationSize.Over.Time[1] <- n0
#	vector.EvenTimes[1] <- 0
#	numeric.CurrentTime <- 0
#	numeric.CurrentPopulationSize <- n0
#	numeric.EventCounter <- 1
#	
#	Prob.Birth <- birth.rate/(birth.rate+death.rate)
#	Prob.Death <- death.rate/(birth.rate+death.rate)
#	
#	while(numeric.CurrentTime <= numeric.StopTime){
#		
#		alpha <- (birth.rate+death.rate)*numeric.CurrentPopulationSize
#		numeric.Random.WaitingTime <-  rexp(n = 1, rate = alpha)
#		numeric.CurrentTime <- numeric.CurrentTime + numeric.Random.WaitingTime
#		state.RandomEvent <- sample(x = c("birth", "death"), size = 1, replace = T, prob = c(Prob.Birth, Prob.Death))
#		
#		if (state.RandomEvent=="birth"){
#			numeric.CurrentPopulationSize =numeric.CurrentPopulationSize + 1
#		}
#		if (state.RandomEvent=="death"){
#			numeric.CurrentPopulationSize =numeric.CurrentPopulationSize -1
#		}
#		
#		numeric.EventCounter <- numeric.EventCounter + 1
#		vector.PopulationSize.Over.Time[numeric.EventCounter] <- numeric.CurrentPopulationSize
#		vector.EvenTimes[numeric.EventCounter] <- numeric.CurrentTime
#		
#	}
#	names(vector.PopulationSize.Over.Time) <- vector.EvenTimes
#	return(vector.PopulationSize.Over.Time)
#}
#
##plot(Simuulate.Birth.Death.Model(n0 = 100, birth.rate = 21/90, death.rate = 20/90, numeric.StopTime = 200), type = "l")
#
####################################
## Generate transition rate matrix #
####################################
#Generate.TransitionRate.Matrix <- function(birth.rate, death.rate, n0, delta.t){
#	matrix.lower <- n0-5
#	matrix.upper <- n0+5
#	possible.PopSizes <- matrix.lower:matrix.upper
#	Matrix.TransitionProbabilities <- matrix(0, nrow = length(possible.PopSizes), ncol = length(possible.PopSizes))
#	rownames(Matrix.TransitionProbabilities) <- colnames(Matrix.TransitionProbabilities) <- possible.PopSizes
#	
#	for (i in 1:length(possible.PopSizes)){
#		Current.PopSize <- rownames(Matrix.TransitionProbabilities)[i]
#		
#		for (j in 1:length(possible.PopSizes)){
#			Next.PopSize <- colnames(Matrix.TransitionProbabilities)[j]
#			
#			if (as.numeric(Next.PopSize) == as.numeric(Current.PopSize)+1){
#				Matrix.TransitionProbabilities[Next.PopSize,Current.PopSize] <- birth.rate*as.numeric(Current.PopSize)*delta.t
#			}
#			if (as.numeric(Next.PopSize) == as.numeric(Current.PopSize)-1){
#				Matrix.TransitionProbabilities[Next.PopSize,Current.PopSize] <- death.rate*as.numeric(Current.PopSize)*delta.t
#			}
#			if (as.numeric(Next.PopSize) == as.numeric(Current.PopSize)){
#				Matrix.TransitionProbabilities[Next.PopSize,Current.PopSize] <-  1-(birth.rate+death.rate)*as.numeric(Current.PopSize)*delta.t
#			}
#			
#			
#		}
#	}
#	return(Matrix.TransitionProbabilities)
#}
#
##Generate.TransitionRate.Matrix(birth.rate = 21/90, 20/90, n0 = 100, delta.t = .001)
#
#'