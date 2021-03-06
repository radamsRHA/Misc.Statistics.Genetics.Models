% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Read.PyRAD.Alleles1.2.R
\name{Read.PyRAD.Alleles1}
\alias{Read.PyRAD.Alleles1}
\title{Read.PyRAD.Alleles1.R Function to read the alignments of a *.alleles file generated by pyRAD (uses all individuals across alignments)}
\usage{
Read.PyRAD.Alleles1(alleles.file, popID)
}
\arguments{
\item{alleles.file}{*.alleles file that is generated by pyRAD}
}
\description{
The function returns a list of alignments obtained from a *.alleles file generated by pyRAD
}
\examples{
library(AlnStats)
library(stringr)

EXAMPLE
S.data <- Calc.S(alleles.data = aln.data)
hist(S.data$theta.hat.vec)
hist(S.data$S.vec)
}
\keyword{genetics,}
\keyword{phylogenetics,}
\keyword{population}
\keyword{pyRAD}
