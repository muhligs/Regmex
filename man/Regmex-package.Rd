\name{Regmex-package}
\alias{Regmex-package}
\alias{Regmex}
\docType{package}
\title{
Regular expression motif explorer. 
}
\description{
Regmex is a flexible motif analysis package that evaluates rank correlation or clustering of motifs in lists of nucleotide sequences.
Motifs can be defined as regular expressions and Regmex evaluates accurate p-values for motif observations in sequences using embedded Markov models.
}
\details{
\tabular{ll}{
Package: \tab Regmex\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-09-11\cr
License: \tab GPL-2\cr
}
The package contains the main functions motif.p() and motif.list.p() for evaluating rank correlation of a motif or a list of motifs in a list of sequences.
}
\author{
Morten Muhlig Nielsen, Paula Tataru

Maintainer:\cr Morten Muhlig Nielsen \cr morten.muhlig@clin.au.dk

}
\references{
Regmex, Motif discovery in ranked lists of sequences. (submitted)
}
~~ O  ptionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
\seealso{
vignette("Regmex")
}
\examples{
data(seqlist)
motif <- "ACG(C|G)TT"
motif.p(seqlist, motif)
}