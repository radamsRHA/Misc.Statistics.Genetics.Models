#' Read.PyRAD.Alleles.2.R Function to read the alignments of a *.alleles file generated by pyRAD (uses all individuals across alignments)
#'
#' The function returns a list of alignments obtained from a *.alleles file generated by pyRAD
#' @param alleles.file *.alleles file that is generated by pyRAD
#' @param popID vector containing all of the samples ID that are extracted from the alignments
#' @keywords population genetics, phylogenetics, pyRAD
#' @export
#' @examples
#' library(AlnStats)
#' library(stringr)
#' 
#' EXAMPLE
#' wanted <- c(">CV0031_0", ">CV0031_1", ">CV0310_0",">CV0310_1")
#' aln.data <- read.alleles2(alleles.file = '~/Desktop/EXAMPLE.alleles', popID = wanted)
#' S.data <- Calc.S(alleles.data = aln.data)
#' hist(S.data$theta.hat.vec)
#' hist(S.data$S.vec)

##################################################################################
################ Read *.alleles file from pyRAD output (version 2)################
##################################################################################


Read.PyRAD.Alleles.2<- function(alleles.file, popID){
	print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	print(gsub(pattern = "XXX", replacement = alleles.file, x = 'Processing file XXX...'))
	infile = readLines(alleles.file) # Open readlines of alleles file
	num.loci <- length(grep('//', readLines(alleles.file))) # Count number of loci in alleles file
	print(gsub(pattern = 'XXX', replacement = num.loci, x = "Found XXX loci..."))
	
	locus.count = 0 # Start locus count
	sample.count = 0 # Init sample count = 0
	
	aln.data = list() # vector used for alignments
	
	locus.aln <- vector()# Init vector to append sequences for each locus independently
	for (line in infile){
		if (str_count(line, "//") == 0){ # If sequence line
			header <- strsplit(line, split = ' ')[[1]][1] # Split pyrad locus format to get header only
			if (header %in% popID){
				locus.aln <- c(locus.aln, line) # append to locus.aln
			}
		}
		sample.count = sample.count + 1 # Up sample count for each line
		if (str_count(line, "//") == 1){ # If the end of a locus is reached
			locus.count = locus.count +1 # Upp the locus count
			m <- length(locus.aln) # Get number of samples at the locus alignment
			n <- length(strsplit(strsplit(locus.aln[[1]], split = ' ')[[1]][3], split = '')[[1]]) # Get length of alignment
			
			locus.aln.matrix <- matrix(nrow = m, ncol = n) # Init locus alignment matrix
			header.list <- rep(NA, m)
			for (sample.sample in 1:m){ # Loop through samples at alignment locus
				
				sample <- locus.aln[[sample.sample]] # Extract sample data at the locus
				header <- strsplit(sample, split = ' ')[[1]][1] # Split pyrad locus format to get header only
				
				if (header %in% popID){
					header.list[sample.sample] <- header # Append sequence header ID
					sequence <- strsplit(sample, split = ' ')[[1]][3] # Split pyrad locus format to get sequence only
					sequence.split <- strsplit(sequence, split = '')[[1]] # Split and extract sequence 
					locus.aln.matrix[sample.sample, ] <- sequence.split # Append sequence to alignment matrix
				}
			}
			
			rownames(locus.aln.matrix) <- header.list # Set rownames
			aln.data[[locus.count]] <- locus.aln.matrix # Append alignment to aln.data list 
			total.sample.count = sample.count # Total sample.count for that locus
			locus.aln <- vector() # Clean the locus.aln vector
		}
	}
	return(aln.data)
}
