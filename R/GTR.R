GTR.Q <- function(d.dist, pi.vec, rr.vec){
  pi.T <- pi.vec["T"]; pi.C <- pi.vec["C"]; pi.A <- pi.vec["A"]; pi.G <- pi.vec["G"]
  a <- rr.vec["a"];   b <- rr.vec["b"];   c <- rr.vec["c"];   d <- rr.vec["d"];   e <- rr.vec["e"];   f <- rr.vec["f"];   g <- rr.vec["g"];   h <- rr.vec["h"];   i <- rr.vec["i"];   j <- rr.vec["j"];   k <- rr.vec["k"];   l <- rr.vec["l"]
  Q <- matrix(nrow = 4, ncol = 4)
  Q[,1] <- c(-d.dist*(a*pi.C+b*pi.G+c*pi.T), d.dist*a*pi.C, d.dist*b*pi.G, d.dist*c*pi.T)
  Q[,2] <- c(d.dist*g*pi.A, -d.dist*(g*pi.A+d*pi.G+e*pi.T), d.dist*d*pi.G, d.dist*e*pi.T)
  Q[,3] <- c(d.dist*h*pi.A, d.dist*i*pi.C, -d.dist*(h*pi.A+j*pi.C+f*pi.T), d.dist*f*pi.T)
  Q[,4] <- c(d.dist*j*pi.A, d.dist*k*pi.C, d.dist*l*pi.G, -d.dist*(i*pi.A+k*pi.C+l*pi.G))
  colnames(Q) <- c("A", "C", "G", "T")
  rownames(Q) <- c("A", "C", "G", "T")
  return(Q)
}


#pi <- rep(.25, 4)
#names(pi) <- c("A", "C", "G", "T")
#rr <- rep(c(2,1,1), 4)
#names(rr) <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l")
#
#sub.rate = 2.2*10^-9
#expm(GTR.Q(.1, pi, rr))
#