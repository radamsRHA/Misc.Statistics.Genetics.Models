SimCoal <- function(n){
print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
  k <- n
  sample.n <- 1:n
  coal.times <- vector()
  coal.history <- vector()
  while (k > 1){
    event.time <- rexp(n = 1, rate = (k*(k-1))/2)
    coal.times <- c(coal.times, event.time)
    k <- k -1
    while (length(sample.n) > 1){
      coal.pair <- sample(sample.n, size = 2)
      coal.history <- c(coal.history, toString(coal.pair))
      sample.n <- sample.n[sample.n != coal.pair[1]]
      sample.n <- sample.n[sample.n != coal.pair[2]]
      pair <- toString(coal.pair)
      pair <- gsub("XX", pair, "(XX)")
#      pair2 <- gsub("X2", coal.pair[2], pair1)
#      pair3 <- gsub("YY", length(sample.n), pair2)
      sample.n <- c(sample.n, (pair))
    }
  }
  coal.timed <- vector()
  coal.timed[1] <- coal.times[1]
  coal.timed[2] <- coal.timed[1] + coal.times[2]
  for (i in 3:length(coal.times)){
    x <- coal.times[i] + coal.timed[i-1]
    coal.timed <- c(coal.timed, x)
  }
  coalescent <- paste(coal.history[n-1], ";")
  coalescent <- paste("(", coalescent)
  coalescent <- gsub(";", ");", coalescent)
  names(coal.history) <- coal.timed
  return(coalescent)
}

plot(read.tree(text = SimCoal(10)))
