##############################################################################
############### Single Population Coalescent Simulation ######################
##############################################################################

#################
# Yang1, pg.315 #
#################

SinglePopCoal.Yang1 <- function(n, l, theta){
print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
  event.times <- vector()
  event.label.list <- vector()
  sample.list <- 1:n
  coalescent.history <- vector()
  k = n
  while (k > 1){# Cycle through sample coalescent events
    event.time.rate <- (k*(k-1)/2)+(k*l*theta/2) # Exponential rate parameter
    random.event.time <- rexp(n = 1, rate = event.time.rate) # Random time sampled from exponential distribution
    p.coalescent <- (k-1)/(k-1+l*theta)
    p.mutation <- 1-p.coalescent
    event <- sample(c("Coalescence", "Mutation"), size = 1, prob = c(p.coalescent, p.mutation)) # Sample events
    event.times <- c(event.times, random.event.time)
    event.label.list <- c(event.label.list, event)
    if (event == "Coalescence"){ # Decrease k by one
      k = k - 1
      coal.pair <- sample(sample.list, size = 2, replace = F)
      coalescent.history <- c(coalescent.history, toString(coal.pair))
      sample.list <- sample.list[sample.list != coal.pair[1]]
      sample.list <- sample.list[sample.list != coal.pair[2]]
      pair <- toString(coal.pair)
      pair <- gsub("XX", pair, "(XX)")
      sample.list <- c(sample.list, (pair))
    }
  }
  coalescent <- paste(coalescent.history[n-1], ";")
  coalescent <- paste("(", coalescent)
  coalescent <- gsub(";", ");", coalescent)
  return(coalescent)

}
#y <- SinglePopCoal.Yang1(10, 10, .1)
#plot(read.tree(text = y))


#################
# Yang2, pg.316 #
#################

SinglePopCoal.Yang2 <- function(n, l, theta){
print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
  event.times <- vector()
  sample.list <- 1:n
  coalescent.history <- vector()
  k = n
  while (k > 1){# Cycle through sample coalescent events
    event.time.rate <- (k*(k-1)/2) # Exponential rate parameter
    random.event.time <- rexp(n = 1, rate = event.time.rate) # Random time sampled from exponential distribution
    event.times <- c(event.times, random.event.time)
    k = k - 1
    coal.pair <- sample(sample.list, size = 2, replace = F)
    coalescent.history <- c(coalescent.history, toString(coal.pair))
    sample.list <- sample.list[sample.list != coal.pair[1]]
    sample.list <- sample.list[sample.list != coal.pair[2]]
    pair <- toString(coal.pair)
    pair <- gsub("XX", pair, "(XX)")
    sample.list <- c(sample.list, (pair))
    }
  coalescent <- paste(coalescent.history[n-1], ";")
  coalescent <- paste("(", coalescent)
  coalescent <- gsub(";", ");", coalescent)
  return(coalescent)
}

#SinglePopCoal.Yang2(10, 10, 1)
