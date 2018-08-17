##############################
# Generate example alignment #
##############################
matrix.Example.Alignment <- matrix(nrow = 5, ncol = 6)
matrix.Example.Alignment[1,] <- c("A", "A", "A", "G", "C", "G")
matrix.Example.Alignment[2,] <- c("A", "A", "A", "G", "T", "G")
matrix.Example.Alignment[3,] <- c("A", "A", "A", "A", "T", "T")
matrix.Example.Alignment[4,] <- c("A", "A", "T", "G", "T", "T")
matrix.Example.Alignment[5,] <- c("A", "A", "T", "G", "T", "T")
rownames(matrix.Example.Alignment) <- c("Sequence1", "Sequence2", "Sequence3", "Sequence4", "Sequence5")

#############################################################
# Summarize features of a coalescent history of n sequences #
#############################################################
SummarizeGenealogy <- function(n){
	vector.TimeIntervals <- paste("T", n:2, sep ="")
	print(gsub(pattern = "XXX", replacement = n-1, x = "Genealogy has XXX coalescent events; one for each node in the tree"))
	print(gsub(pattern = "XXX", replacement = n-1, x = "Genealogy has XXX coalescent time intervals: Ti = T1, T2,.. Tn-1; one for each epoch"))
	print(c("A set of time intervals Ti:", vector.TimeIntervals))
	
	print(gsub(pattern = "XXX", replacement = (2*n)-3, x = "Genealogy has a total of XXX branches (unrooted)"))
	print(gsub(pattern = "XXX", replacement = (2*n)-3, x = "Genealogy has a total of XXX branches (rooted)"))
	print(gsub(pattern = "XXX", replacement = n, x = "Genealogy has XXX external branches"))
	print(gsub(pattern = "XXX", replacement = n-2, x = "Genealogy has XXX internal branches that partition the sample into subsets with at least two members"))
}

###########################################################
# Compute Watterson's 1975 S: number of segregating sites #
###########################################################
Compute.Watterson1975.S <- function(matrix.Alignment){
	numeric.LocusLength <- length(matrix.Alignment[1,])
	numeric.NumberOfSamples <- length(rownames(matrix.Alignment))
	Watterson1975.S <- 0
	
	for (i in 1:numeric.LocusLength){
		vector.SiteSpecificAlign <- matrix.Alignment[,i]
		if (length(unique(x = vector.SiteSpecificAlign)) == 2){
			Watterson1975.S <- Watterson1975.S + 1
		}
	}
	print(gsub(pattern = "XXX", replacement = numeric.NumberOfSamples, x = "n = XXX"))
	print(gsub(pattern = "XXX", replacement = numeric.LocusLength, x = "L = XXX"))
	print(gsub(pattern = "XXX", replacement = Watterson1975.S, x = "Watterson's 1975 S = XXX"))
}

##################################################################################
# Compute average number of differences between pairs of sequences in the sample #
##################################################################################
Compute.Average.Pairwise.Difference.pi <- function(matrix.Alignment){
	numeric.LocusLength <- length(matrix.Alignment[1,])
	numeric.NumberOfSamples <- length(rownames(matrix.Alignment))
	numeric.SummationOfDifferences <- 0
	vector.Pairwise.Differences <- vector()
	
	for (i in 1:(numeric.NumberOfSamples-1)){
		for (j in (i+1):numeric.NumberOfSamples){
			
			################################################
			# extract the two sequences from the alignment #
			################################################
			vector.Sequence.i <- matrix.Alignment[i,]
			vector.Sequence.j <- matrix.Alignment[j,]
			matrix.PairWise.Alignment <- matrix(nrow = 2, ncol = numeric.LocusLength)
			matrix.PairWise.Alignment[1,] <- vector.Sequence.i
			matrix.PairWise.Alignment[2,] <- vector.Sequence.j
			numeric.NumberOfDifferences.ij <- 0
			
			for (s in 1:numeric.LocusLength){
				vector.SiteSpecificAlignment <- matrix.PairWise.Alignment[,s]
				
				if (length(unique(x = vector.SiteSpecificAlignment)) == 2){
					numeric.NumberOfDifferences.ij <- numeric.NumberOfDifferences.ij + 1
				}
			}
			numeric.SummationOfDifferences <- numeric.SummationOfDifferences + numeric.NumberOfDifferences.ij
			vector.Pairwise.Differences <- c(vector.Pairwise.Differences, numeric.NumberOfDifferences.ij)
		}
	}
	
	Average.Pairwise.Difference.pi <- (1/(choose(n = numeric.NumberOfSamples, k = 2))) * numeric.SummationOfDifferences
	print(gsub(pattern = "XXX", replacement = Average.Pairwise.Difference.pi, x = "Average Pairwise Differences: XXX"))
	print(gsub(pattern = "XXX", replacement = mean(vector.Pairwise.Differences), x = "Average Pairwise Differences: XXX"))
}

##########################################
# Compute Folded site-frequency spectrum #
##########################################
Compute.Folded.SFS <- function(matrix.Alignment){
	numeric.NumberOfSamples <- length(rownames(matrix.Alignment))
	numeric.LocusLength <- length(matrix.Alignment[1,])
	
	#############################
	# find largest possible SFS #
	#############################
	if ((numeric.NumberOfSamples %% 2) == 0){
		numeric.Largest.SFS <- numeric.NumberOfSamples/2
	} 
	if ((numeric.NumberOfSamples %% 2) != 0){
		numeric.Largest.SFS <- (numeric.NumberOfSamples-1)/2
	} 
	
	###############
	# Compute SFS #
	###############
	numeric.TotalNumber.Possible.SF <- length(1:numeric.Largest.SFS)
	vector.Folded.SFS <- rep(0, numeric.TotalNumber.Possible.SF)
	names(vector.Folded.SFS) <- paste("N_", 1:numeric.Largest.SFS, sep = "")
	
	for (i in 1:numeric.Largest.SFS){
		n.i <- i # number of sites with one base in i copies
		n.NOT.i <- numeric.NumberOfSamples - n.i # the other base in n-i copies
		
		for (j in 1:numeric.LocusLength){
			
			vector.SiteSpecificAln <- matrix.Alignment[,j]
			table.SitePatterns <- table(vector.SiteSpecificAln)
			
			if (length(x = grep(pattern = n.i, x = table.SitePatterns)) == 1){
				if (length(x = grep(pattern = n.NOT.i, x = table.SitePatterns)) == 1){
					vector.Folded.SFS[i] <- vector.Folded.SFS[i] + 1
				}
			}
		}
	}
	
	##############
	# format SFS #
	##############
	for (i in 1:numeric.Largest.SFS){
		n.i <- i # number of sites with one base in i copies
		n.NOT.i <- numeric.NumberOfSamples - n.i # the other base in n-i copies
		
		if (n.i == n.NOT.i){
			vector.Folded.SFS[i] <- vector.Folded.SFS[i]/(1+1)
		}
	}
	return(vector.Folded.SFS)
}

#############################################
# Compute Wattersons's 1975 S using the SFS #
#############################################
Compute.Watterson175.S.With.SFS <- function(matrix.Alignment){
	vector.SFS <- Compute.Folded.SFS(matrix.Alignment = matrix.Alignment)
	Watterson1975.S <- 0 
	
	for (i in 1:length(vector.SFS)){
		Watterson1975.S <- Watterson1975.S + vector.SFS[i]
	}
	print(gsub(pattern = "XXX", replacement = Watterson1975.S, x = "Watterson's 1975 S = XXX"))	
}

#####################################################
# Compute average pairwisedistance pi using the SFS #
#####################################################
Compute.Average.Pairwise.Difference.pi.WITH.SFS <- function(matrix.Alignment){
	numeric.NumberOfSamples <- length(rownames(matrix.Alignment))
	vector.SFS <- Compute.Folded.SFS(matrix.Alignment = matrix.Alignment)
	numeric.SummationOfDifferences <- 0
	
	for (i in 1:length(vector.SFS)){
		numeric.SummationOfDifferences <- numeric.SummationOfDifferences + (i*(numeric.NumberOfSamples-i)*vector.SFS[i])
	}
	Average.Pairwise.Difference.pi <- (1/(choose(n = numeric.NumberOfSamples, k = 2))) * numeric.SummationOfDifferences	
	print(Average.Pairwise.Difference.pi)
	
}

