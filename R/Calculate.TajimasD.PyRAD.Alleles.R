#' Calculate.TajimasD.PyRAD.Alleles.R
#'
#' The function returns a list of D test for each locus provided
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
################ Get D test results ####################################################################
########################################################################################################

Calculate.TajimasD.PyRAD.Alleles <- function(alleles.data){
	print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	num.loci <- length(alleles.data) # Get number of loci
	D.vec <- rep(NA, num.loci) # vector to collect D estimates
	D.P.norm <- rep(NA, num.loci) # vector to collect P values (assuing normal)
	D.P.beta <- rep(NA, num.loci) # vector to collect P values (assuing beta)
	
	D.data.list <- list()
	for (locus in 1:num.loci){ # Loop through loci
		locus.aln <- alleles.data[[locus]] # Extract locus alignment
		n <- length(locus.aln[1,]) # get number of sites
		m <- length(locus.aln) # Number of samples for this locus
		
		D.locus = tajima.test(as.DNAbin(as.alignment(locus.aln))) # Conduct Tajima's D test
		
		D.vec[locus] <- D.locus$D
		D.P.norm[locus] <- D.locus$Pval.normal
		D.P.beta[locus] <- D.locus$Pval.beta
	}
	
	return(list(D.vec = D.vec, D.P.norm = D.P.norm, D.P.beta = D.P.beta))
}
