# Kngen = Markov process sampling from binomial distribution each generation
# prints vector with "Moments of generation 1" for the dynamics of the system
# returns "collectp" vector of p from each generation stipulated by ngen
# Example: plot(Kngen(100,100,.9), type = "l")


Kngen <- function(ngen, N, p){
print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
  collectp <- vector()
  EK1 <- N*p
  VarK1 <- N*p*(1-p)
  for (i in 1:ngen){
    p <- rbinom(1, N, p)/N
    collectp <- c(collectp, p)
  }
  MomentsGen1 <- c(EK1, VarK1)
  names(MomentsGen1) <- c("Expected number of alleles in generation 1", "Variance in number of alleles in generation 1")
  print(MomentsGen1)
  return(collectp)
}

# Hngen = Markov process sampling from the joint binomial distribution of p and q each generation
# prints vector with "Moments of generation 1" and "Expected hetero after ngen" for the dynamics of the system
# returns "collectH" vector containing Heterozygosity of the population each generation
# Example: plot(Hngen(100,100,.9), typle = "l)

Hngen <- function(ngen, N, p){
print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
  collectH <- vector()
  pi <- p
  Hi <- 2*p*q
  EH1 <- Hi *(1-(1/N))
  EHngen <- Hi*exp(-(ngen/N))
  for (i in 1:ngen){
    pi <- rbinom(1, N, pi)/N
    Hi <- 2*pi*(1-pi)
    collectH <- c(collectH, Hi)
  }
  EH <- c(EH1, EHngen)
  names(EH) <- c("Expected hetero after 1 gen", gsub("XXX", ngen, "Expected hetero after XXX gen"))
  print(EH)
  return(collectH)
}


