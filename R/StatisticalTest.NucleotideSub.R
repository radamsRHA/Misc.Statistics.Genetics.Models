############
## Simulate data
############
#Simulate.UnderBroadH1 <- function(number.N, number.n, vector.prob){
#  matrix.Aln <- matrix(nrow = number.n, ncol = number.N)
#  
#  for (i in 1:number.N){
#    matrix.Aln[,i] <- sample(x = c("A", "C", "G", "T"), size = number.n, replace = T, prob = vector.prob)
#  }
#  return(matrix.Aln)
#}
#
#
#
#Compute.Like <- function(number.N, number.n, vector.prob){
#  matrix.Aln <- matrix(nrow = number.n, ncol = number.N)
#  
#  for (i in 1:number.N){
#    matrix.Aln[,i] <- sample(x = c("A", "C", "G", "T"), size = number.n, replace = T, prob = vector.prob)
#  }
#  
#  matrix.UniqueColumns <- unique(matrix.Aln, MARGIN=2)
#  number.UniqueColumns <- length(matrix.UniqueColumns[1,])
#  vector.TableUniqueObserv <- rep(NA, number.UniqueColumns)
#  vector.Names <- rep(NA, number.UniqueColumns)
#  
#  for (i in 1:number.UniqueColumns){
#    vector.UniqueColumn <- matrix.UniqueColumns[,i]
#    counter.UniqueObserv <- 0
#    for (j in 1:number.N){
#      vector.MatrixAlnData <- matrix.Aln[,j]
#      if (all.equal(vector.MatrixAlnData, vector.UniqueColumn) == T){
#        counter.UniqueObserv = counter.UniqueObserv + 1
#        
#      }
#    }
#    vector.Names[i] <- toString(vector.UniqueColumn)
#    vector.TableUniqueObserv[i] <- counter.UniqueObserv
#  }
#  
#  names(vector.TableUniqueObserv) <- vector.Names
#  I <- iterpc(n = 4, r = number.n, replace = T, labels = c("A", "C", "G", "T"), ordered = T)
#  matrix.TotalSetUnique <- getall(I)
#  number.TotalSetUnique <- length(matrix.TotalSetUnique[,1])
#  number.DataLikelihood <- 1
#  number.DataLogLike <- 0
#  
#  for (i in 1:number.TotalSetUnique){
#    vector.UniquePattern <- matrix.TotalSetUnique[i,]
#    number.ObservedFreqUniquePattern <- vector.TableUniqueObserv[toString(vector.UniquePattern)]
#      if (is.na(number.ObservedFreqUniquePattern) == T){
#        number.ObservedFreqUniquePattern <- number.N
#      } else{ print(number.ObservedFreqUniquePattern)}
#   number.Like.i <- as.numeric(number.ObservedFreqUniquePattern)/number.N
#   number.LogLike.i <- number.N * log(as.numeric(number.ObservedFreqUniquePattern)) - number.N * log(as.numeric(number.N))
#   number.DataLikelihood <- as.numeric(number.DataLikelihood)*number.Like.i
#   number.DataLogLike <- number.DataLogLike + number.LogLike.i
#  }
#  
#  print(number.DataLikelihood)
#  print(number.DataLogLike)
#}
#
#
#
#Compute.Like(number.N = 10, number.n = 5, vector.prob = c(0.90, 0.025, 0.05, 0.025))
#
############
## use real data
############
#
#Compute.DataLikeData <- function(matrix.Aln){
#  number.N <- length(matrix.Aln[1,])
#  number.n <- length(matrix.Aln[,1])
#  
# 
#  matrix.UniqueColumns <- unique(matrix.Aln, MARGIN=2)
#  number.UniqueColumns <- length(matrix.UniqueColumns[1,])
#  vector.TableUniqueObserv <- rep(NA, number.UniqueColumns)
#  vector.Names <- rep(NA, number.UniqueColumns)
#  
#  for (i in 1:number.UniqueColumns){
#    vector.UniqueColumn <- matrix.UniqueColumns[,i]
#    counter.UniqueObserv <- 0
#    for (j in 1:number.N){
#      vector.MatrixAlnData <- matrix.Aln[,j]
#      if (all.equal(vector.MatrixAlnData, vector.UniqueColumn) == T){
#        counter.UniqueObserv = counter.UniqueObserv + 1
#        
#      }
#    }
#    vector.Names[i] <- toString(toupper(vector.UniqueColumn))
#    vector.TableUniqueObserv[i] <- counter.UniqueObserv
#  }
#  
#  names(vector.TableUniqueObserv) <- vector.Names
#  I <- iterpc(n = 5, r = number.n, replace = T, labels = c("A", "C", "G", "T", '-'), ordered = T)
#  #matrix.TotalSetUnique <- getall(I)
#  matrix.TotalSetUnique <- getall(I)
#  #number.TotalSetUnique <- 5^number.n
#  number.TotalSetUnique <- length(matrix.TotalSetUnique[,1])
#  number.DataLikelihood <- 1
#  number.DataLogLike <- 0
#  
#  for (i in 1:number.TotalSetUnique){
#    #vector.UniquePattern <-getnext(I)
#    vector.UniquePattern <- matrix.TotalSetUnique[i,]
#    number.ObservedFreqUniquePattern <- vector.TableUniqueObserv[toString(vector.UniquePattern)]
#    if (is.na(number.ObservedFreqUniquePattern) == T){
#      number.ObservedFreqUniquePattern <- number.N
#    } else{ print(number.ObservedFreqUniquePattern)}
#    #number.Like.i <- as.numeric(number.ObservedFreqUniquePattern)/number.N
#    number.LogLike.i <- number.N * log(as.numeric(number.ObservedFreqUniquePattern)) - number.N * log(as.numeric(number.N))
#    #number.DataLikelihood <- as.numeric(number.DataLikelihood)*number.Like.i
#    number.DataLogLike <- number.DataLogLike + number.LogLike.i
#  }
#  
#  #print(number.DataLikelihood)
#  return(number.DataLogLike)
#}
#
##matrix.Aln <- Simulate.UnderBroadH1(number.N = 10, number.n = 5, vector.prob = c(0.90, 0.025, 0.05, 0.025))
#
#Compute.DataLikeData(matrix.Aln = matrix.Aln)
#
############
## use real data example
############
#
#Compute.RealData <- function(matrix.Aln){
#  number.N <- length(matrix.Aln[1,])
#  number.n <- length(matrix.Aln[,1])
#  
#  
#  matrix.UniqueColumns <- unique(matrix.Aln, MARGIN=2)
#  number.UniqueColumns <- length(matrix.UniqueColumns[1,])
#  vector.TableUniqueObserv <- rep(NA, number.UniqueColumns)
#  vector.Names <- rep(NA, number.UniqueColumns)
#  
#  for (i in 1:number.UniqueColumns){
#    vector.UniqueColumn <- matrix.UniqueColumns[,i]
#    counter.UniqueObserv <- 0
#    for (j in 1:number.N){
#      vector.MatrixAlnData <- matrix.Aln[,j]
#      if (all.equal(vector.MatrixAlnData, vector.UniqueColumn) == T){
#        counter.UniqueObserv = counter.UniqueObserv + 1
#        
#      }
#    }
#    vector.Names[i] <- toString(toupper(vector.UniqueColumn))
#    vector.TableUniqueObserv[i] <- counter.UniqueObserv
#  }
#  
#  names(vector.TableUniqueObserv) <- vector.Names
#  I <- iterpc(n = 5, r = number.n, replace = T, labels = c("A", "C", "G", "T", '-'), ordered = T)
#  #matrix.TotalSetUnique <- getall(I)
#  #matrix.TotalSetUnique <- getall(I)
#  number.TotalSetUnique <- 5^number.n
#  #number.TotalSetUnique <- length(matrix.TotalSetUnique[,1])
#  number.DataLikelihood <- 1
#  number.DataLogLike <- 0
#  
#  for (i in 1:number.TotalSetUnique){
#    vector.UniquePattern <-getnext(I)
#    #vector.UniquePattern <- matrix.TotalSetUnique[i,]
#    number.ObservedFreqUniquePattern <- vector.TableUniqueObserv[toString(vector.UniquePattern)]
#    if (is.na(number.ObservedFreqUniquePattern) == T){
#      number.ObservedFreqUniquePattern <- number.N
#    } else{ print(number.ObservedFreqUniquePattern)}
#    #number.Like.i <- as.numeric(number.ObservedFreqUniquePattern)/number.N
#    number.LogLike.i <- number.N * log(as.numeric(number.ObservedFreqUniquePattern)) - number.N * log(as.numeric(number.N))
#    #number.DataLikelihood <- as.numeric(number.DataLikelihood)*number.Like.i
#    number.DataLogLike <- number.DataLogLike + number.LogLike.i
#  }
#  
#  #print(number.DataLikelihood)
#  return(number.DataLogLike)
#}
#
#library(PartitionFiction)
#library(ape)
#
#results.handle.InputAlignment <- read.nexus.data(file = '~/Desktop/EXAMPLE/Delsuc_2003/alignment.nex')
#results.matrix.PartitionSpecificData <- Read.Best_Scheme(path.best_scheme = '~/Desktop/FUN/analysis/best_scheme.txt')
#
#results.list.ExtractPartitionSpecificDataAln <- Extract.PartitionSpecificDataAln(handle.InputAlignment = results.handle.InputAlignment, matrix.PartitionSpecificData = results.matrix.PartitionSpecificData$matrix.PartitionSpecificData)
#matrix.Aln <- results.list.ExtractPartitionSpecificDataAln$list.ExtractPartitionSpecificDataAln[[1]]$matrix.PartitionSpecificAlignment
#
#
#results <- Compute.RealData(matrix.Aln = matrix.Aln)
#
#
#