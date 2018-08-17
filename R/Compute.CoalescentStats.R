# plot expected coalescent times across a range of n (number of samples)
# Example:
#plot(ETi(10), type = "l", main= gsub("XXX", 10, "Expected time for 2 copies to coalesce in sample size of XXX to XXX -1"),
#xlab=gsub("XXX", 10, "Number of samples, XXX to XXX-1 coalescent events"), ylab = "Expected coalescent time (in N generations)",xaxt='n')
#axis(1,at = 1:9,labels=names(ETi(10)),las=2)

ETi <- function(n){
  collectE <- vector()
  for (i in n:2){
    ETi <- 2/(i*(i-1))
    collectE <- c(collectE, ETi)
  }
  names(collectE) <- n:2
  return(collectE)
}

#plot variance in coalescent times across a range of n (number of samples)
# Example:
#plot(VarTi(10), type = "l", main= gsub("XXX", 10, "Variance in coalescen times for 2 copies to coalesce in sample size of XXX to XXX -1"),
#     xlab=gsub("XXX", 10, "Number of samples, XXX to XXX-1 coalescent events"), ylab = "Variance in  coalescent time (in N generations)",xaxt='n')
#axis(1,at = 1:9,labels=names(VarTi(10)),las=2)
     
VarTi <- function(n){
  collectVar <- vector()
  for (i in n:2){
    VarTi <- (2/(i*(i-1)))^2
    collectVar <- c(collectVar, VarTi)
  }
  names(collectVar) <- n:2
  return(collectVar)
}

