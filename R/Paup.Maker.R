library(ape)
library(phylotools)
library(phangorn)


PaupMaker <- function(path.ParentDir){
	print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	vector.Subdirs_full <- list.files(path.ParentDir, full.names = T)
	vector.Subdirs_short <- list.files(path.ParentDir, full.names = F)
	number.Subdirs <- length(vector.Subdirs_short)
	
	for (i in 1:number.Subdirs){
		path.Subdir_full.i <- vector.Subdirs_full[i]
		path.Subdir_short.i <- vector.Subdirs_short[i]
		
		path.SubAnalysisDir <- paste(path.Subdir_full.i, '/', "PartitionPaupFiction_Analysis_Aug30", sep = "")
		unlink(path.SubAnalysisDir, recursive = T)
		dir.create(path.SubAnalysisDir)
		
		path.SubAnalysisWITHOUTDir <- paste(path.SubAnalysisDir, "/", "Without-PF/", sep = "")
		dir.create(path.SubAnalysisWITHOUTDir)
		path.SubAnalysisWITHDir <- paste(path.SubAnalysisDir, "/", "With-PF/", sep = "")
		dir.create(path.SubAnalysisWITHDir)
		
		path.OLDAlignment <- paste(path.Subdir_full.i, '/', "alignment.nex", sep = "")	
		path.WithoutAlignment.nexus <- paste(path.SubAnalysisWITHOUTDir, "/", path.Subdir_short.i, '_alignment.nexus', sep = "")
		path.WithAlignment.nexus <- paste(path.SubAnalysisWITHDir, "/", path.Subdir_short.i, '_alignment.nexus', sep = "")
		
		path.WithoutAlignment.phylip <- paste(path.SubAnalysisWITHOUTDir, "/", path.Subdir_short.i, '_alignment.phylip', sep = "")
		path.WithAlignment.phylip <- paste(path.SubAnalysisWITHDir, "/", path.Subdir_short.i, '_alignment.phylip', sep = "")
		
		
		file.copy(from = path.OLDAlignment, to = path.WithoutAlignment.nexus)
		file.copy(from = path.OLDAlignment, to = path.WithAlignment.nexus)
		
		handle.OLDAlignment <- read.nexus.data(file = path.OLDAlignment)
		handle.OLDAlignment.PhyDat <- phyDat(handle.OLDAlignment)
		write.phyDat(handle.OLDAlignment.PhyDat, file = path.WithoutAlignment.phylip, format="phylip")
		write.phyDat(handle.OLDAlignment.PhyDat, file = path.WithAlignment.phylip, format="phylip")
		
		path.WithoutConfig <- paste(path.SubAnalysisWITHOUTDir, '/', "partition_finder.cfg", sep = "")
		path.WithConfig <- paste(path.SubAnalysisWITHDir, '/', "partition_finder.cfg", sep = "")
		
		
		string.alignment <- paste("alignment = ", path.Subdir_short.i, '_alignment.phylip;', sep = "")
		
		write(x = "## ALIGNMENT FILE ##", file = path.WithoutConfig, append = T)
		write(x = string.alignment, file = path.WithoutConfig, append = T)
		write(x = "\n", file = path.WithoutConfig, append = T)
		write(x = "## BRANCHLENGTHS ##\nbranchlengths = linked;\n\n## MODELS OF EVOLUTION ##\nmodels = mrbayes;\nmodel_selection = BIC;", 
					file = path.WithoutConfig, append = T)
		write(x = "\n## DATA BLOCKS: see manual for how to define ##\n[data_blocks]\n", file = path.WithoutConfig, append = T)
		
		
		write(x = "## ALIGNMENT FILE ##", file = path.WithConfig, append = T)
		write(x = string.alignment, file = path.WithConfig, append = T)
		write(x = "\n", file = path.WithConfig, append = T)
		write(x = "## BRANCHLENGTHS ##\nbranchlengths = linked;\n\n## MODELS OF EVOLUTION ##\nmodels = mrbayes;\nmodel_selection = BIC;", 
					file = path.WithConfig, append = T)
		write(x = "\n## DATA BLOCKS: see manual for how to define ##\n[data_blocks]\n", file = path.WithConfig, append = T)
		

		handle.ReadLines.OLDAlignment <- readLines(path.OLDAlignment)
		
		vector.collectschemes <- "user = "
		for (line in handle.ReadLines.OLDAlignment){
			if (length(grep(pattern = "CHARSET", x = line)) ==1 ){
				if (length(grep(pattern = "mitochondrial_genome", x = line)) == 0 && length(grep(pattern = "nuclear_genome", x = line))==0){
					string.SplitLine <- strsplit(x = line, split = "CHARSET")[[1]][2]
					string.x <- gsub(pattern = "\t", replacement = "", x = string.SplitLine)
					write(x = string.x, file = path.WithoutConfig, append = T)
					write(x = string.x, file = path.WithConfig, append = T)
					string.y <- strsplit(x = string.x, split = " = ")[[1]][1]
					vector.collectschemes <- paste(vector.collectschemes, " (", string.y, ")", sep = "")
					
					
				}
			}
		}
		write(x = "\n\n## SCHEMES, search: all | user | greedy ##\n[schemes]\nsearch = user;\n\n# user schemes", file = path.WithoutConfig, append = T)
		write(x = paste(vector.collectschemes, ';', sep = ""), file = path.WithoutConfig, append = T)
		
		write(x = "\n\n## SCHEMES, search: all | user | greedy ##\n[schemes]\nsearch = greedy;\n\n# user schemes", file = path.WithConfig, append = T)
		
		

	}
	
}


#PaupMaker(path.ParentDir = '~/Desktop/BenchMarkAlignements_Aug30/')




