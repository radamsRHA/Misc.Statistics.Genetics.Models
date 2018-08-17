#' Calculate.S.PyRAD.Alleles.R
#'
#' The function returns two lists: (1) the number of segregating sites (2) estimate of S across all loci (See pg. 97 in Wakeley)
#' @param alleles.data list for each locus that is returned from the function 'read.alleles' 
#' @param popID String to identify the particular population used to compute S statistics
#' @keywords population genetics, phylogenetics, pyRAD
#' @export
#' @examples
#' library(AlnStats)
#' library(stringr)
#' 
#' EXAMPLE
#' aln.data <- read.alleles(alleles.file = '~/Desktop/EXAMPLE.alleles')

########################################################################################################
################ Get mutation information (number of mutations, S estimates) ###########################
########################################################################################################


Calculate.S.PyRAD.Alleles <- function(alleles.data){
	print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	num.loci <- length(alleles.data) # Get number of loci
	theta.hat.vec <- rep(NA, num.loci) # Init theta.hat.vec
	S.vec <- rep(NA, num.loci) # Init S vec
	
	
	for (locus in 1:num.loci){ # Loop through loci
		locus.aln <- alleles.data[[locus]] # Extract locus alignment
		n <- length(locus.aln[1,]) # get number of sites
		m <- length(locus.aln) # Number of samples for this locus

		a1 <- sum(1/(1:(m-1))) # a1 stat
		
		S.count = 0
		for (site in 1:n){ # Loop through sites in alignment
			site.aln <- locus.aln[,site] # Extract site column
			
			if (length(unique(site.aln)) > 1 && length(grep(pattern = "N", x = site.aln)) == 0 && length(grep(pattern = '-', x = site.aln)) == 0){ # If variable site
				S.count = S.count +1 # Up variable site count
			}
			
		}
		
		Ps <- S.count/n # Proportion of different sites
		theta.hat <- Ps/a1 # Estimate of theta
		
		theta.hat.vec[locus] <- theta.hat
		S.vec[locus] <- S.count
	}
	return(list(theta.hat.vec = theta.hat.vec, S.vec = S.vec))
}

