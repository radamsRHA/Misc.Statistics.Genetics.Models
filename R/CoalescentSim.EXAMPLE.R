TreeHeightLogger = 0.01
theta0 = 0.01
theta1 = 0.01
theta2 = 0.01



#species.model <- sptree<-"(A:TreeHeightLogger#theta0, B:TreeHeightLogger#theta1)#theta2;" # Iniatilze species model with blank parameters
#species.model <- gsub(pattern = "TreeHeightLogger", replacement = TreeHeightLogger, x = species.model) # Set TreeHeight in model [Tau]  for MCMC step estimate
#species.model <- gsub(pattern = "theta0", replacement = theta0, x = species.model) # Set theta0 for MCMC step estimate
#species.model <- gsub(pattern = "theta1", replacement = theta1, x = species.model) # Set theta1 for MCMC step estimate
#species.model <- gsub(pattern = "theta2", replacement = theta2, x = species.model) # Set theta2 for MCMC step estimate

#Simulated.Locus.Aln <- simSeqfromSp2(simsequence = 0, sptree = species.model, spname = c('A','B'), outfile = '', ntaxasp = c(12,12), ngene = 1) 


#t <- read.tree(text = Simulated.Locus.Aln$gt)
#plot(t)
