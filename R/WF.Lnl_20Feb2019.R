K <- 10
vector.n <- rep(10, K)
vector.qk <- runif(n = K, min = 0, max = 1)
vector.Sample.y <- rbinom(n = K, size = vector.n, prob = vector.qk)



Function.WF.Lnl <- function(vector.y, vector.q, vector.n){
  
  
  K <- length(vector.y)
  MLE <- 0
  
  for (i in 1:K){
    
    yi <- vector.y[i]
    qi <- vector.q[i]
    ni <- vector.n[i]
    
    MLE <- MLE + log(choose(n = ni, k = yi) * (qi^yi) * ((1-qi)^(ni-yi)))
  }
  return(MLE)
}


Function.WF.Lnl(vector.y = vector.Sample.y, vector.q = vector.qk, vector.n = vector.n)

vector.qk <- vector.qk + rnorm(n = length(vector.qk), mean = 0, sd = 0.01)
Function.WF.Lnl(vector.y = vector.Sample.y, vector.q = vector.qk, vector.n = vector.n)
