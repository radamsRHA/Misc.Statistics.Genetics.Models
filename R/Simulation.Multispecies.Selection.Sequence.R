# Define function for samples with selection
#library(ape)
#Sim.Select <- function(p, N, ngen, w.list){
#  wAA <- w.list[1]
#  wAa <- w.list[2]
#  waa <- w.list[3]
#  for (gen in 1:ngen){
#    # Before selection
#    p <- rbinom(1,N, p)/N
#    fAA <- p^2
#    fAa <- 2*p*(1-p)
#    faa <- (1-p)^2
#    
#    # After selection
#    w.bar <- fAA*wAA+fAa*wAa+faa*waa
#    
#    fAA.prime <- fAA*wAA/w.bar
#    fAa.prime <- fAa*wAa/w.bar
#    faa.prime <- faa*waa/w.bar
#    
#    p.prime <- fAA.prime+fAa.prime/2
#    p <- p.prime
#  }
#  return(p.prime)
#}
#
#
#MS.Sim <- function(N, ngen, w.list1, w.list2, w.list3){
#  ABC.pop <- sample(c("A", "a"), N, prob = c(.5,.5), replace = T)
#  ABC.p <- table(ABC.pop)["A"]/N
#  AB.p <- rbinom(1, N, ABC.p)/N
#  C.p <- rbinom(1, N, ABC.p)/N
#  for (gen in 1:ngen){
#    AB.p <- rbinom(1, N, AB.p)/N
#    C.p <- rbinom(1, N, C.p)/N
#  }
#  A.p <- rbinom(1, N, AB.p)/N
#  B.p <- rbinom(1, N, AB.p)/N
#  C.p <- rbinom(1, N, C.p)/N
#
#  A.p <- Sim.Select(A.p, N, ngen, w.list1)
#  B.p <- Sim.Select(B.p, N, ngen, w.list2)
#  C.p <- Sim.Select(C.p, N, ngen, w.list3)
#  return(list(v1 = A.p, v2 = B.p, v3 = C.p))
#}
#
#
## Next, add functin with proportion of loci under selection and number of loci, convert to nexus format
#
#Sim.Genome.Samples <- function(n, m, N, ngen, w.list1, w.list2, w.list3){
#  # n/3 must be true
#  dat.matrix <- matrix(nrow = n, ncol = m)
#  set.names <- vector()
#  for (xx in 1:(n/3)){
#    samp.name <- gsub('XXX', xx, 'SpeciesA_XXX')
#    set.names <- c(set.names, samp.name)
#  }
#  for (xx in 1:(n/3)){
#    samp.name <- gsub('XXX', xx, 'SpeciesB_XXX')
#    set.names <- c(set.names, samp.name)
#  }
#  for (xx in 1:(n/3)){
#    samp.name <- gsub('XXX', xx, 'SpeciesC_XXX')
#    set.names <- c(set.names, samp.name)
#  }
#  row.names(dat.matrix) <- set.names
#  
#  for (locus in 1:m){
#    MS.simulate <- MS.Sim(N, ngen, w.list1, w.list2, w.list3)
#    pop.A <- c(rep("A", MS.simulate[[1]]*N), rep("a", (1-MS.simulate[[1]])*N))
#    pop.B <- c(rep("A", MS.simulate[[2]]*N), rep("a", (1-MS.simulate[[2]])*N))
#    pop.C <- c(rep("A", MS.simulate[[3]]*N), rep("a", (1-MS.simulate[[3]])*N))
#    SpeciesA <- vector()
#    SpeciesB <- vector()
#    SpeciesC <- vector()
#    
#    for (hap.sam in 1:(n/3)){
#      geno.A <- sample(pop.A, 2)
#      geno.B <- sample(pop.B, 2)
#      geno.C <- sample(pop.C, 2)
#      if (geno.A[1] == "A" & geno.A[2] == "A"){
#        genotype.A = 0
#      }
#      if (geno.A[1] == "A" & geno.A[2] == "a"){
#        genotype.A = 1
#      }
#      if (geno.A[1] == "a" & geno.A[2] == "A"){
#        genotype.A = 1
#      }
#      if (geno.A[1] == "a" & geno.A[2] == "a"){
#        genotype.A = 2
#      }
#      ####
#      if (geno.B[1] == "A" & geno.B[2] == "A"){
#        genotype.B = 0
#      }
#      if (geno.B[1] == "A" & geno.B[2] == "a"){
#        genotype.B = 1
#      }
#      if (geno.B[1] == "a" & geno.B[2] == "A"){
#        genotype.B = 1
#      }
#      if (geno.B[1] == "a" & geno.B[2] == "a"){
#        genotype.B = 2
#      }
#      ###
#      if (geno.C[1] == "A" & geno.C[2] == "A"){
#        genotype.C = 0
#      }
#      if (geno.C[1] == "A" & geno.C[2] == "a"){
#        genotype.C = 1
#      }
#      if (geno.C[1] == "a" & geno.C[2] == "A"){
#        genotype.C = 1
#      }
#      if (geno.C[1] == "a" & geno.C[2] == "a"){
#        genotype.C = 2
#      }
#      
#      SpeciesA <- c(SpeciesA, genotype.A)
#      SpeciesB <- c(SpeciesB, genotype.B)
#      SpeciesC <- c(SpeciesC, genotype.C)
#    }
#    site.dat <- c(SpeciesA, SpeciesB, SpeciesC)
#    dat.matrix[,locus] <- site.dat
#    }
#  return(dat.matrix)
#}
#
#
#
#Sim.Genome.Final <- function(n, m, N, ngen, w.list1, w.list2, w.list3, p.select, outfile){
#  neutral.dat <- Sim.Genome.Samples(n, m-p.select, N, ngen, c(1, 1, 1), c(1, 1, 1), c(1, 1, 1))
#  select.dat <- Sim.Genome.Samples(n, p.select, N, ngen, w.list1, w.list2, w.list3)
#  final.dat <- cbind(neutral.dat, select.dat)
#  list.seq <- list()
#  for (i in 1:n){
#    list.seq[[i]] <- final.dat[i,]
#  }
#  names(list.seq) <- row.names(final.dat)
#  write.nexus.data(list.seq, file = outfile, interleaved = F)
#}
#
#
## Simulate 4 null rounds
#print("start")
#ptm <- proc.time()
#Sim.Genome.Final(9, 1000, 100000, 100000,c(1, 1, 1), c(1, 1, 1), c(1, 1, 1), 5, '/home/castoelab/Desktop/SelectionMSC/Simulation_Trial2_Oct14/Neutral/Neutral1.nex')
#print("finish")
#y = proc.time() - ptm
#print(y)
#Sim.Genome.Final(9, 1000, 100000, 100000,c(1, 1, 1), c(1, 1, 1), c(1, 1, 1), 5, '/home/castoelab/Desktop/SelectionMSC/Simulation_Trial2_Oct14/Neutral/Neutral2.nex')
#Sim.Genome.Final(9, 1000, 100000, 100000,c(1, 1, 1), c(1, 1, 1), c(1, 1, 1), 5, '/home/castoelab/Desktop/SelectionMSC/Simulation_Trial2_Oct14/Neutral/Neutral3.nex')
#Sim.Genome.Final(9, 1000, 100000, 100000,c(1, 1, 1), c(1, 1, 1), c(1, 1, 1), 5, '/home/castoelab/Desktop/SelectionMSC/Simulation_Trial2_Oct14/Neutral/Neutral4.nex')
#
#print("null done")
## Simulate 4 select rounds at 10%
#Sim.Genome.Final(9, 1000, 100000, 100000,c(1, 1, .90), c(1, 1, 1), c(1, 1, 1), 100, '/home/castoelab/Desktop/SelectionMSC/Simulation_Trial2_Oct14/Positive/S_90/Percent10/Selection_90_10_1.nex')
#Sim.Genome.Final(9, 1000, 100000, 100000,c(1, 1, .90), c(1, 1, 1), c(1, 1, 1), 100, '/home/castoelab/Desktop/SelectionMSC/Simulation_Trial2_Oct14/Positive/S_90/Percent10/Selection_90_10_2.nex')
#Sim.Genome.Final(9, 1000, 100000, 100000,c(1, 1, .90), c(1, 1, 1), c(1, 1, 1), 100, '/home/castoelab/Desktop/SelectionMSC/Simulation_Trial2_Oct14/Positive/S_90/Percent10/Selection_90_10_3.nex')
#Sim.Genome.Final(9, 1000, 100000, 100000,c(1, 1, .90), c(1, 1, 1), c(1, 1, 1), 100, '/home/castoelab/Desktop/SelectionMSC/Simulation_Trial2_Oct14/Positive/S_90/Percent10/Selection_90_10_4.nex')
#print("first select down")
## Simulate 4 select rounds at 20%
#Sim.Genome.Final(9, 1000, 100000, 100000,c(1, 1, .90), c(1, 1, 1), c(1, 1, 1), 250, '/home/castoelab/Desktop/SelectionMSC/Simulation_Trial2_Oct14/Positive/S_90/Percent25/Selection_90_25_1')
#Sim.Genome.Final(9, 1000, 100000, 100000,c(1, 1, .90), c(1, 1, 1), c(1, 1, 1), 250, '/home/castoelab/Desktop/SelectionMSC/Simulation_Trial2_Oct14/Positive/S_90/Percent25/Selection_90_25_2')
#Sim.Genome.Final(9, 1000, 100000, 100000,c(1, 1, .90), c(1, 1, 1), c(1, 1, 1), 250, '/home/castoelab/Desktop/SelectionMSC/Simulation_Trial2_Oct14/Positive/S_90/Percent25/Selection_90_25_3')
#Sim.Genome.Final(9, 1000, 100000, 100000,c(1, 1, .90), c(1, 1, 1), c(1, 1, 1), 250, '/home/castoelab/Desktop/SelectionMSC/Simulation_Trial2_Oct14/Positive/S_90/Percent25/Selection_90_25_4')
#print('second')
#
## Simulate 4 select rounds at 10%
#Sim.Genome.Final(9, 1000, 100000, 100000,c(1, 1, .99), c(1, 1, 1), c(1, 1, 1), 100, '/home/castoelab/Desktop/SelectionMSC/Simulation_Trial2_Oct14/Positive/S_75/Percent10/Selection_75_10_1.nex')
#Sim.Genome.Final(9, 1000, 100000, 100000,c(1, 1, .99), c(1, 1, 1), c(1, 1, 1), 100, '/home/castoelab/Desktop/SelectionMSC/Simulation_Trial2_Oct14/Positive/S_75/Percent10/Selection_75_10_2.nex')
#Sim.Genome.Final(9, 1000, 100000, 100000,c(1, 1, .99), c(1, 1, 1), c(1, 1, 1), 100, '/home/castoelab/Desktop/SelectionMSC/Simulation_Trial2_Oct14/Positive/S_75/Percent10/Selection_75_10_3.nex')
#Sim.Genome.Final(9, 1000, 100000, 100000,c(1, 1, .99), c(1, 1, 1), c(1, 1, 1), 100, '/home/castoelab/Desktop/SelectionMSC/Simulation_Trial2_Oct14/Positive/S_75/Percent10/Selection_75_10_4.nex')
#print('thid')
## Simulate 4 select rounds at 25%
#Sim.Genome.Final(9, 1000, 100000, 100000,c(1, 1, .99), c(1, 1, 1), c(1, 1, 1), 250, '/home/castoelab/Desktop/SelectionMSC/Simulation_Trial2_Oct14/Positive/S_75/Percent25/Selection_75_25_1.nex')
#Sim.Genome.Final(9, 1000, 100000, 100000,c(1, 1, .99), c(1, 1, 1), c(1, 1, 1), 250, '/home/castoelab/Desktop/SelectionMSC/Simulation_Trial2_Oct14/Positive/S_75/Percent25/Selection_75_25_2.nex')
#Sim.Genome.Final(9, 1000, 100000, 100000,c(1, 1, .99), c(1, 1, 1), c(1, 1, 1), 250, '/home/castoelab/Desktop/SelectionMSC/Simulation_Trial2_Oct14/Positive/S_75/Percent25/Selection_75_25_3.nex')
#Sim.Genome.Final(9, 1000, 100000, 100000,c(1, 1, .99), c(1, 1, 1), c(1, 1, 1), 250, '/home/castoelab/Desktop/SelectionMSC/Simulation_Trial2_Oct14/Positive/S_75/Percent25/Selection_75_25_4.nex')
#
#
#print("all done")
#
#
#
#