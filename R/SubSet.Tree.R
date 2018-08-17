GetSubtreeNode <- function(tree, subtree){
print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	given.subtree <- subtree
	for (x in 1:length(subtrees(tree))){
		sub.tree <- subtrees(tree)[[x]]
		if (length(sub.tree$tip.label) == length(given.subtree$tip.label)){
			return(sub.tree$name)
		}
	}
}


