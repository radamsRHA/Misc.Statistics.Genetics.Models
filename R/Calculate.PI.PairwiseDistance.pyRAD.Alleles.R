#' Calculate.PI.PairwiseDistance.pyRAD.Alleles.R
#'
#' The function returns a list of pi for each locus provided
#' @param alleles.data list for each locus that is returned from the function 'read.alleles' 
#' @keywords population genetics, phylogenetics, pyRAD
#' @export
#' @examples
#' library(AlnStats)
#' library(stringr)
#' library(pegas)
#' 
#' EXAMPLE
#' aln.data <- read.alleles(alleles.file = '~/Desktop/EXAMPLE.alleles')

########################################################################################################
################ Get nucleotide diversity across loci within a a population ############################
########################################################################################################

Calculate.PI.PairwiseDistance.pyRAD.Alleles <- function(alleles.data){
	print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	num.loci <- length(alleles.data) # Get number of loci
	pi.vec <- rep(NA, num.loci) # vector to collect pi estimates
	
	for (locus in 1:num.loci){ # Loop through loci
		locus.aln <- alleles.data[[locus]] # Extract locus alignment
		n <- length(locus.aln[1,]) # get number of sites
		m <- length(locus.aln) # Number of samples for this locus
		
		pi.locus = nuc.div(as.DNAbin(as.alignment(locus.aln)))
		pi.vec[locus] <- pi.locus
	}
	
	return(pi.vec)
}

