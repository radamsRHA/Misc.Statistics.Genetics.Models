#' Read.Directory.PhylipFiles.R Function to read the alignments from a directory of phylip alignments
#'
#' The function returns a list of alignments obtained from a directory that contains phylip formatted alignments
#' @param phy.dir directory containing phylip formats
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


Read.Directory.PhylipFiles <- function(phy.dir, popID){
	print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	print(gsub(pattern = "XXX", replacement = phy.dir, x = 'Processing directory XXX...'))
	infiles = dir(phy.dir, pattern =".phy") # Get alignment list
	
	aln.data = list() # vector used for alignments
	num.loci <- length(infiles) # Get number of loci
	print(gsub(pattern = 'XXX', replacement = num.loci, x = "Found XXX loci..."))
	
	locus.count = 0
	for (file in 1:num.loci){
		locus.count = locus.count +1
		infile = gsub(pattern = "XXX", replacement = infiles[file], x = 'YYYXXX')
		infile = gsub(pattern = "YYY", replacement = phy.dir, x = infile)
		
		infile.dna <- read.dna(file = infile) # Read as DNA.bin
		locus.aln <- as.alignment(infile.dna) # Read as alignment
		string.split.aln <- strsplit(locus.aln$seq, split = '') # Split sequences
		n <- length(string.split.aln[[1]]) # Get number of sites
		
		header.list = locus.aln$nam
		locus.list = locus.aln$seq
		
		locus.aln.matrix <- matrix(nrow = 20, ncol = n)
		
		header.count = 0
		for (h in 1:(length(header.list))){ # for each sample.header
			header = header.list[h] # Extract header
			
			header.count = header.count +1
			sample.sequece = locus.list[h] # Extract sequence
			extract.seq <- strsplit(sample.sequece, split = '') # Split sequence
			locus.aln.matrix[header.count,] <- extract.seq[[1]]
		}
		print(locus.aln.matrix)
		aln.data[[locus.count]] <- locus.aln.matrix # Append alignment to aln.data list 
	}
	return(aln.data)
}