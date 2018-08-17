library(stringr)

#################################################
# GetSample data works similar to ParseAlleles: returns a list of 3:
# Alleles data (same as parse alleles)
# Mutation data: number of mutations for each locus
# Sample data: number of samples present at each locus
#################################################

GetSampleData <- function(allele.file){
print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	num.loci <- length(grep('//', readLines(allele.file)))
	infile = readLines(allele.file)
	locus.count = 1
	sample.count = 0
	alleles.data = vector(mode="list", length=num.loci)
	sample.data = vector()
	mut.data = vector()
	for (line in infile){
		sample.count = sample.count + 1
		if (str_count(line, "//") == 1){
			total.sample.count = sample.count
			k = str_count(line, '\\*') + str_count(line, '-')
			alleles.data[[locus.count]] <- c(toString(total.sample.count-1), toString(k))
			sample.data <- c(sample.data, total.sample.count)
			mut.data <- c(mut.data, k)
			locus.count = locus.count + 1
			sample.count = 0
		}
	}
	return(list(loci.dat = alleles.data, mutation.data = mut.data, sample.data = sample.data))
}


################################################
################ Infinite sites model 
################################################

Coal.sim <- function(n.list, theta){
print("please cite the following when using these functions: Adams, R.H., Schield, D.R., Card, D.C., Corbin, A., Castoe, T.A., 2017. ThetaMater: Bayesian estimation of population size parameter h from genomic data. Bioinformatics 34, 1072–1073.")
	n = as.numeric(sample(n.list, size = 1))
	mut.vec <- vector()
	k = n
	mut.count = 0
	while (k > 1) {
		p.mut = (theta)/(k-1+(theta))
		p.coal = 1-p.mut
		event = sample(c("M", "C"), size = 1, prob = c(p.mut, p.coal))
		if (event == "C"){
			k = k-1
		}
		if (event == "M"){
			mut.count = mut.count + 1
			mut.vec[mut.count] = "M"
		}
	}
	return(length(mut.vec))
}



#########################
##MLE of theta for CVOS #
#########################
#con.1 <- theta.like.pyRAD('~/Desktop/ROUND1/CON_1.alleles', bound = 2, interval = 0.001)
#hell.1 <- theta.like.pyRAD('~/Desktop/ROUND1/HELL_1.alleles', bound = 2, interval = 0.001)
#lut.1 <- theta.like.pyRAD('~/Desktop/ROUND1/LUT_1.alleles', bound = 2, interval = 0.001)
#nun.1 <- theta.like.pyRAD('~/Desktop/ROUND1/NUN_1.alleles', bound = 2, interval = 0.001)
#ore.1 <- theta.like.pyRAD('~/Desktop/ROUND1/ORE_1.alleles', bound = 2, interval = 0.001)
#vir.1 <- theta.like.pyRAD('~/Desktop/ROUND1/VIR_1.alleles', bound = 2, interval = 0.001)
#cerb.1 <- theta.like.pyRAD('~/Desktop/ROUND1/CON_1.alleles', bound = 2, interval = 0.001)
#
#
#par(mfrow=c(7,3))
###################
#### CONCOLOR #####
###################
#con.MLE.theta <- con.1[con.1 == max(con.1)]
#con.data <- GetSampleData(allele.file = '~/Desktop/ROUND1/CON_1.alleles')
#con.mut <- con.data[[2]]
#con.sample <- con.data[[3]]
#
#CON.sim.fun <- replicate(100000, Coal.sim(n.list= con.sample, theta = as.numeric(names(con.MLE.theta))))
#plot(con.1[2:length(con.1)], type = "l", xaxt = 'n', main = gsub("XXX", as.numeric(names(con.MLE.theta)), "CONCOLOR: θ = XXX"),
#		 xlab = "θ", ylab = "-ln")
#axis(side = 1, at = seq(0, length(con.1), 100), labels = (seq(0, length(con.1), 100)/1000))
#abline(v = as.numeric(names(con.MLE.theta))*1000, col = "red", lty = 2, lwd = 2)
#hist(con.mut, breaks = 100, xlab = "# mutations", main = "Empirical distribution of S", ylab = "# loci")
#hist(CON.sim.fun, breaks = 100, xlab = "# mutations", main = "Simulated distribution of S given MLE(θ)", ylab = "# loci")
#
###################
#### HELL #########
###################
#hell.MLE.theta <- hell.1[hell.1 == max(hell.1)]
#hell.data <- GetSampleData(allele.file = '~/Desktop/ROUND1/HELL_1.alleles')
#hell.mut <- hell.data[[2]]
#hell.sample <- hell.data[[3]]
#HELL.sim.fun <- replicate(100000, Coal.sim(n.list= hell.sample, theta = as.numeric(names(hell.MLE.theta))))
#
#plot(hell.1[2:length(hell.1)], type = "l", xaxt = 'n', main = gsub("XXX", as.numeric(names(hell.MLE.theta)), "HELLERI: θ = XXX"),
#		 xlab = "θ", ylab = "-ln")
#axis(side = 1, at = seq(0, length(hell.1), 100), labels = (seq(0, length(hell.1), 100)/1000))
#abline(v = as.numeric(names(hell.MLE.theta))*1000, col = "red", lty = 2, lwd = 2)
#hist(hell.mut, breaks = 100, xlab = "# mutations", main = "Empirical distribution of S", ylab = "# loci")
#hist(HELL.sim.fun, breaks = 100, xlab = "# mutations", main = "Simulated distribution of S given MLE(θ)", ylab = "# loci")
#
###################
#### LUT #########
###################
#lut.MLE.theta <- lut.1[lut.1 == max(lut.1)]
#lut.data <- GetSampleData(allele.file = '~/Desktop/ROUND1/LUT_1.alleles')
#lut.mut <- lut.data[[2]]
#lut.sample <- lut.data[[3]]
#
#LUT.sim.fun <- replicate(100000, Coal.sim(n.list= lut.sample, theta = as.numeric(names(lut.MLE.theta))))
#
#plot(lut.1[2:length(lut.1)], type = "l", xaxt = 'n', main = gsub("XXX", as.numeric(names(lut.MLE.theta)), "LUTOSUS: θ = XXX"),
#		 xlab = "θ", ylab = "-ln")
#axis(side = 1, at = seq(0, length(lut.1), 100), labels = (seq(0, length(lut.1), 100)/1000))
#abline(v = as.numeric(names(lut.MLE.theta))*1000, col = "red", lty = 2, lwd = 2)
#hist(lut.mut, breaks = 100, xlab = "# mutations", main = "Empirical distribution of S", ylab = "# loci")
#hist(LUT.sim.fun, breaks = 100, xlab = "# mutations", main = "Simulated distribution of S given MLE(θ)", ylab = "# loci")
#
###################
#### NUN #########
###################
#nun.MLE.theta <- nun.1[nun.1 == max(nun.1)]
#nun.data <- GetSampleData(allele.file = '~/Desktop/ROUND1/NUN_1.alleles')
#nun.mut <- nun.data[[2]]
#nun.sample <- nun.data[[3]]
#mean(nun.mut)
#NUN.sim.fun <- replicate(100000, Coal.sim(n.list= nun.sample, theta = as.numeric(names(nun.MLE.theta))))
#
#plot(nun.1[2:length(nun.1)], type = "l", xaxt = 'n', main = gsub("XXX", as.numeric(names(nun.MLE.theta)), "NUNTIUS: θ = XXX"),
#		 xlab = "θ", ylab = "-ln")
#axis(side = 1, at = seq(0, length(nun.1), 100), labels = (seq(0, length(nun.1), 100)/1000))
#abline(v = as.numeric(names(nun.MLE.theta))*1000, col = "red", lty = 2, lwd = 2)
#hist(nun.mut, breaks = 100, xlab = "# mutations", main = "Empirical distribution of S", ylab = "# loci")
#hist(NUN.sim.fun, breaks = 100, xlab = "# mutations", main = "Simulated distribution of S given MLE(θ)", ylab = "# loci")
#
#
###################
#### ORE #########
###################
#ore.MLE.theta <- ore.1[ore.1 == max(ore.1)]
#ore.data <- GetSampleData(allele.file = '~/Desktop/ROUND1/ORE_1.alleles')
#ore.mut <- ore.data[[2]]
#ore.sample <- ore.data[[3]]
#
#ORE.sim.fun <- replicate(100000, Coal.sim(n.list= ore.sample, theta = as.numeric(names(ore.MLE.theta))))
#
#plot(ore.1[2:length(ore.1)], type = "l", xaxt = 'n', main = gsub("XXX", as.numeric(names(ore.MLE.theta)), "OREGANUS: θ = XXX"),
#		 xlab = "θ", ylab = "-ln")
#axis(side = 1, at = seq(0, length(ore.1), 100), labels = (seq(0, length(ore.1), 100)/1000))
#abline(v = as.numeric(names(ore.MLE.theta))*1000, col = "red", lty = 2, lwd = 2)
#hist(ore.mut, breaks = 100, xlab = "# mutations", main = "Empirical distribution of S", ylab = "# loci")
#hist(ORE.sim.fun, breaks = 100, xlab = "# mutations", main = "Simulated distribution of S given MLE(θ)", ylab = "# loci")
#
###################
#### VIR #########
###################
#vir.MLE.theta <- vir.1[vir.1 == max(vir.1)]
#vir.data <- GetSampleData(allele.file = '~/Desktop/ROUND1/VIR_1.alleles')
#vir.mut <- vir.data[[2]]
#vir.sample <- vir.data[[3]]
#
#VIR.sim.fun <- replicate(100000, Coal.sim(n.list= vir.sample, theta = as.numeric(names(vir.MLE.theta))))
#
#plot(vir.1[2:length(vir.1)], type = "l", xaxt = 'n', main = gsub("XXX", as.numeric(names(vir.MLE.theta)), "VIRIDIS: θ = XXX"),
#		 xlab = "θ", ylab = "-ln")
#axis(side = 1, at = seq(0, length(vir.1), 100), labels = (seq(0, length(vir.1), 100)/1000))
#abline(v = as.numeric(names(vir.MLE.theta))*1000, col = "red", lty = 2, lwd = 2)
#hist(vir.mut, breaks = 100, xlab = "# mutations", main = "Empirical distribution of S", ylab = "# loci")
#hist(VIR.sim.fun, breaks = 100, xlab = "# mutations", main = "Simulated distribution of S given MLE(θ)", ylab = "# loci")
#
###################
#### CERB #########
###################
#cerb.MLE.theta <- cerb.1[cerb.1 == max(cerb.1)]
#cerb.data <- GetSampleData(allele.file = '~/Desktop/ROUND1/CERB_1.alleles')
#cerb.mut <- cerb.data[[2]]
#cerb.sample <- cerb.data[[3]]
#
#CERB.sim.fun <- replicate(100000, Coal.sim(n.list= cerb.sample, theta = as.numeric(names(cerb.MLE.theta))))
#
#plot(cerb.1[2:length(cerb.1)], type = "l", xaxt = 'n', main = gsub("XXX", as.numeric(names(cerb.MLE.theta)), "CERBERUS: θ = XXX"),
#		 xlab = "θ", ylab = "-ln")
#axis(side = 1, at = seq(0, length(cerb.1), 100), labels = (seq(0, length(cerb.1), 100)/1000))
#abline(v = as.numeric(names(cerb.MLE.theta))*1000, col = "red", lty = 2, lwd = 2)
#hist(cerb.mut, breaks = 100, xlab = "# mutations", main = "Empirical distribution of S", ylab = "# loci", yaxt="n")
#axis(side = 2, at =  seq(0, max(max(table(cerb.mut))), 50), labels = (seq(0, max(max(table(cerb.mut))), 50)/round(sum(table(cerb.mut)), digits = -3)*100))
#hist(CERB.sim.fun, breaks = 100, xlab = "# mutations", main = "Simulated distribution of S given MLE(θ)", ylab = "# loci", yaxt = "n")
#axis(side = 2, at =  seq(0, max(max(table(CERB.sim.fun))), 1000), labels = (seq(0, max(max(table(CERB.sim.fun))), 1000)/round(sum(table(CERB.sim.fun)), digits = -3)*100))
#
###################################################################
#pdf('~/Desktop/fun.pdf', width=10, height=30)
#par(mfrow=c(7,3))
#
#plot(con.1[2:length(con.1)], type = "l", xaxt = 'n', main = gsub("XXX", as.numeric(names(con.MLE.theta)), "CONCOLOR: theta = XXX"),
#		 xlab = "theta", ylab = "-ln")
#axis(side = 1, at = seq(0, length(con.1), 100), labels = (seq(0, length(con.1), 100)/1000))
#abline(v = as.numeric(names(con.MLE.theta))*1000, col = "red", lty = 2, lwd = 2)
#hist(con.mut, breaks = 100, xlab = "# mutations", main = "Empirical distribution of S", ylab = "# loci", yaxt = "n")
#axis(side = 2, at =  seq(0, max(max(table(con.mut))), 50), labels = signif((seq(0, max(max(table(con.mut))), 50)/signif(sum(table(con.mut)), digits = -2)), digits = -3)*100)
#
#hist(CON.sim.fun, breaks = 100, xlab = "# mutations", main = "Simulated distribution of S given MLE(theta)", ylab = "# loci")
#
#plot(hell.1[2:length(hell.1)], type = "l", xaxt = 'n', main = gsub("XXX", as.numeric(names(hell.MLE.theta)), "HELLERI: theta = XXX"),
#		 xlab = "theta", ylab = "-ln")
#axis(side = 1, at = seq(0, length(hell.1), 100), labels = (seq(0, length(hell.1), 100)/1000))
#abline(v = as.numeric(names(hell.MLE.theta))*1000, col = "red", lty = 2, lwd = 2)
#hist(hell.mut, breaks = 100, xlab = "# mutations", main = "Empirical distribution of S", ylab = "# loci")
#hist(HELL.sim.fun, breaks = 100, xlab = "# mutations", main = "Simulated distribution of S given MLE(theta)", ylab = "# loci")
#
#plot(lut.1[2:length(lut.1)], type = "l", xaxt = 'n', main = gsub("XXX", as.numeric(names(lut.MLE.theta)), "LUTOSUS: theta = XXX"),
#		 xlab = "theta", ylab = "-ln")
#axis(side = 1, at = seq(0, length(lut.1), 100), labels = (seq(0, length(lut.1), 100)/1000))
#abline(v = as.numeric(names(lut.MLE.theta))*1000, col = "red", lty = 2, lwd = 2)
#hist(lut.mut, breaks = 100, xlab = "# mutations", main = "Empirical distribution of S", ylab = "# loci")
#hist(LUT.sim.fun, breaks = 100, xlab = "# mutations", main = "Simulated distribution of S given MLE(theta)", ylab = "# loci")
#
#plot(nun.1[2:length(nun.1)], type = "l", xaxt = 'n', main = gsub("XXX", as.numeric(names(nun.MLE.theta)), "NUNTIUS: theta = XXX"),
#		 xlab = "theta", ylab = "-ln")
#axis(side = 1, at = seq(0, length(nun.1), 100), labels = (seq(0, length(nun.1), 100)/1000))
#abline(v = as.numeric(names(nun.MLE.theta))*1000, col = "red", lty = 2, lwd = 2)
#hist(nun.mut, breaks = 100, xlab = "# mutations", main = "Empirical distribution of S", ylab = "# loci")
#hist(NUN.sim.fun, breaks = 100, xlab = "# mutations", main = "Simulated distribution of S given MLE(theta)", ylab = "# loci")
#
#plot(ore.1[2:length(ore.1)], type = "l", xaxt = 'n', main = gsub("XXX", as.numeric(names(ore.MLE.theta)), "OREGANUS: theta = XXX"),
#		 xlab = "theta", ylab = "-ln")
#axis(side = 1, at = seq(0, length(ore.1), 100), labels = (seq(0, length(ore.1), 100)/1000))
#abline(v = as.numeric(names(ore.MLE.theta))*1000, col = "red", lty = 2, lwd = 2)
#hist(ore.mut, breaks = 100, xlab = "# mutations", main = "Empirical distribution of S", ylab = "# loci")
#hist(ORE.sim.fun, breaks = 100, xlab = "# mutations", main = "Simulated distribution of S given MLE(theta)", ylab = "# loci")
#
#plot(vir.1[2:length(vir.1)], type = "l", xaxt = 'n', main = gsub("XXX", as.numeric(names(vir.MLE.theta)), "VIRIDIS: theta = XXX"),
#		 xlab = "theta", ylab = "-ln")
#axis(side = 1, at = seq(0, length(vir.1), 100), labels = (seq(0, length(vir.1), 100)/1000))
#abline(v = as.numeric(names(vir.MLE.theta))*1000, col = "red", lty = 2, lwd = 2)
#hist(vir.mut, breaks = 100, xlab = "# mutations", main = "Empirical distribution of S", ylab = "# loci")
#hist(VIR.sim.fun, breaks = 100, xlab = "# mutations", main = "Simulated distribution of S given MLE(theta)", ylab = "# loci")
#
#plot(cerb.1[2:length(cerb.1)], type = "l", xaxt = 'n', main = gsub("XXX", as.numeric(names(cerb.MLE.theta)), "CERBERUS: theta = XXX"),
#		 xlab = "theta", ylab = "-ln")
#axis(side = 1, at = seq(0, length(cerb.1), 100), labels = (seq(0, length(cerb.1), 100)/1000))
#abline(v = as.numeric(names(cerb.MLE.theta))*1000, col = "red", lty = 2, lwd = 2)
#hist(cerb.mut, breaks = 100,  xlab = "# mutations", main = "Empirical distribution of S", ylab = "# loci")
#hist(CERB.sim.fun, breaks = 100, xlab = "# mutations", main = "Simulated distribution of S given MLE(theta)", ylab = "# loci")
#
#?hist
#dev.off()
#
#