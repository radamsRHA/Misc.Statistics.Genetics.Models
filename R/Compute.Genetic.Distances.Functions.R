Ln.d.JC69 <- function(n, x, d){
  pijmutations <- x*log((1/16)-((1/16)*exp(-4*d/3)))
  piimutations <- (n-x)*log((1/16)+((3/16)*exp(-4*d/3)))
  ln.d <- pijmutations + piimutations
  return(ln.d)
}


Ln.d.K80 <- function(n, x, S, V, d){
  k <- (2*((log(1-(2*S)-V)/log(1-(2*V))))) - 1
  p0 <- ((1/4)+((1/4)*exp(-4*d/(k+2))))+((1/2)*exp(-2*d*(k+1)/(k+2)))
  p1 <- ((1/4)+((1/4)*exp(-4*d/(k+2))))-((1/2)*exp(-2*d*(k+1)/(k+2)))
  p2 <- ((1/4)-((1/4)*exp(-4*d/(k+2))))
  ns = S*n
  nv = V*n
  t1 <- (n-ns-nv)*(log(p0/4))
  t2 <- ns*log(p1/4)
  t3 <- nv*log(p2/4)
  ln.d <- t1 +t2 + t3
}

LRT.ntmodels <- function(n, x, S, V){
  JC69 <- vector()
  K80 <- vector()
  for (t in seq(0, 2, 0.001)){
    jc.x <- Ln.d.JC69(n, x, t)
    k.y <- Ln.d.K80(n, x, S, V, t)
    JC69 <- c(JC69, jc.x )
    K80 <- c(K80, k.y)
  }
  MLE.JC69 <- max(JC69)
  MLE.K80 <- max(K80)
  ltr <- 2*(MLE.K80-MLE.JC69)
  print(ltr)
  p <- pchisq(q = ltr, df = 1, lower.tail=FALSE)
  ret <- c(ltr, p)
  names(ret) <- c("ltr", "p-value")
  return(ret)
}

#LRT.ntmodels(948, 90, 0.08861, 0.00633)

