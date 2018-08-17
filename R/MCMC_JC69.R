Ln.JC69 <- function(n, x, theta){
  t1 <- ((3/4)-((3/4)*exp(-4*theta/3)))^x
  t2 <- ((1/4)+((3/4)*exp(-4*theta/3)))^(n-x)
  x <- t1*t2
  return(x)
}

MCMC.JC69 <- function(n, x, w, ngen){
  states <- vector()
  state = 0.2
  u = .2
  ln.state <- Ln.JC69(n, x, state)
  f.theta <- (1/u)*(exp(-state/u))
  for (gen in 1:ngen){
    ln.state <- Ln.JC69(n, x, state)
    f.theta <- (1/u)*(exp(-state/u))
    u <- runif(1, min=0, max=1)
    new.theta <- state-(w/2)+(w*u)
    ln.new.theta <- Ln.JC69(n, x, new.theta)
    f.new.theta <- (1/u)*exp(-(new.theta)/u)
    print(f.theta)
    ratio <- (f.new.theta*ln.new.theta)/(f.theta * ln.state)
    a.ratio <- min(1, ratio)
    u2 <- runif(1, min=0, max=1)
    if (u2 > a.ratio){
      state = new.theta
    }
    states <- c(states, state)
  }
  return(states)
}

#x <- MCMC.JC69(948, 90, .1, 10000)
#plot(x, type = "l")
#