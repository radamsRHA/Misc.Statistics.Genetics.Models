#FTmrca <- function(n, t){
#  z <- vector()
#  for (i in 2:n){
#    a <- ((2*i)-1)*(-1)^i
#    b <- prod(n:(n-i+1))
#    c <- prod(n:(n+i-1))
#    d <- (choose(i,2)*exp(-(choose(i,2))*t))
#    x <- ((a*b)/c) * d
#    z <- c(z, x)
#  }
#  return(sum(z))
#}
#
#
#z <- vector()
#for (t in seq(0,4, 0.01)){
#  x <- FTmrca(2,t)
#  z <- c(z, x)
#}
#
##plot(z, type = "l", main = "Function of Tmrca given a specific sample size (color indicates larger sample sizes", xlab = "time units", ylab = "probabilty of Tmrca",col = 1)
#
#
#z <- vector()
#for (t in seq(0,4, 0.01)){
#  x <- FTmrca(5,t)
#  z <- c(z, x)
#}
#
##lines(z, col = 2)
#
#z <- vector()
#for (t in seq(0,4, 0.01)){
#  x <- FTmrca(10,t)
#  z <- c(z, x)
#}
#
##lines(z, col = 3)
#
#z <- vector()
#for (t in seq(0,4, 0.01)){
#  x <- FTmrca(20,t)
#  z <- c(z, x)
#}
#
##lines(z, col = 4)