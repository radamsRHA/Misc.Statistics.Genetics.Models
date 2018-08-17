#library(phylosim)
#
## Clock like tree
#cat("((SpeciesA:0.001,SpeciesB:0.001):0.001,SpeciesC:0.002);",file="~/Desktop/Clock.tree")
#
#
## non-Clock like tree
#cat("((SpeciesA:0.001,SpeciesB:10):0.001,SpeciesC:0.001);",file="~/Desktop/NonClock.tree")
#
#fitch.relative.rate <- function(tree.path, l){
#	
#	# simulated on tree
#	sim.results <- Simulate(PhyloSim(
#		root.seq=sampleStates( NucleotideSequence(len=l,proc=list(list(JC69())) )),
#		phylo=read.tree(tree.path)
#	))
#	
#	d.AC <- 0
#	d.BC <- 0
#	d.AB <- 0
#
#	AC.aln.matrix <- matrix(nrow = 2, ncol = 1000)
#	rownames(AC.aln.matrix) <- c("SpeciesA", "SpeciesC")
#	
#	BC.aln.matrix <- matrix(nrow = 2, ncol = 1000)
#	rownames(BC.aln.matrix) <- c("SpeciesB", "SpeciesC")
#	
#	AB.aln.matrix <- matrix(nrow = 2, ncol = 1000)
#	rownames(BC.aln.matrix) <- c("SpeciesA", "SpeciesB")
#	
#
#	AC.aln.matrix[1,] <- sim.results$alignment[rownames(sim.results$alignment) == "SpeciesA"]
#	AC.aln.matrix[2,] <- sim.results$alignment[rownames(sim.results$alignment) == "SpeciesC"]
#	
#	BC.aln.matrix[1,] <- sim.results$alignment[rownames(sim.results$alignment) == "SpeciesB"]
#	BC.aln.matrix[2,] <- sim.results$alignment[rownames(sim.results$alignment) == "SpeciesC"]
#	
#	AB.aln.matrix[1,] <- sim.results$alignment[rownames(sim.results$alignment) == "SpeciesA"]
#	AB.aln.matrix[2,] <- sim.results$alignment[rownames(sim.results$alignment) == "SpeciesB"]
#	
#	
#	for (i in 1:l){
#		AC.site.aln <- AC.aln.matrix[,i] # Extract site alignment
#		BC.site.aln <- BC.aln.matrix[,i] # Extract site alignment
#		AB.site.aln <- AB.aln.matrix[,i] # Extract site alignment
#		
#		if (length(unique(AC.site.aln))>=2){
#			d.AC = d.AC + 1
#		}
#		if (length(unique(BC.site.aln))>=2){
#			d.BC = d.BC + 1
#		}
#	
#		if (length(unique(AB.site.aln))>=2){
#			d.AB = d.AB + 1
#		}
#	}
#	
#	d.AC = d.AC/l
#	d.BC = d.BC/l
#	d.AB = d.AB/l
#	
#	print(gsub(pattern = 'XXX', replacement = d.AC, x = "distance between A-C: XXX"))
#	print(gsub(pattern = 'XXX', replacement = d.BC, x = "distance between B-C: XXX"))
#	print(gsub(pattern = 'XXX', replacement = d.AB, x = "distance between A-B: XXX"))
#	
#	a = (d.AB+d.AC-d.BC)/2
#	b = (d.AB+d.BC-d.AC)/2
#	
#	chi.square <- ((a-b)^2)/(a+b)
#	
#	print(chi.square)
#}
#
## Clock Like test
#fitch.relative.rate(tree.path = '~/Desktop/Clock.tree', l = 1000)
#fitch.relative.rate(tree.path = '~/Desktop/NonClock.tree', l = 1000)
#
#
#