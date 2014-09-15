\name{align.motif}
\alias{align.motif}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{function to display the motifs occurring in a set of sequences alligned by the motifs start base.
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
align.motif(motif, seqlist, lf, rf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{motif}{
%%     ~~Describe \code{motif} here~~
}
  \item{seqlist}{
%%     ~~Describe \code{seqlist} here~~
}
  \item{lf}{
%%     ~~Describe \code{lf} here~~
}
  \item{rf}{
%%     ~~Describe \code{rf} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (motif, seqlist, lf, rf) 
{
    vec.res <- c()
    for (i in seqlist) {
        vec <- gregexpr(motif, i$sequence)[[1]]
        if (vec[1] == -1) 
            next
        for (k in vec) {
            vec.res <- c(vec.res, substr(i$sequence, k - lf, 
                k + nchar(motif) + rf - 1))
        }
    }
    return(vec.res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
