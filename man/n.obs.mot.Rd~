\name{n.obs.pat}
\alias{n.obs.pat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{a function returning the number of observed motifs in a sequence.
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
n.obs.pat(pattern, seq, overlap = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pattern}{
%%     ~~Describe \code{pattern} here~~
}
  \item{seq}{
%%     ~~Describe \code{seq} here~~
}
  \item{overlap}{
%%     ~~Describe \code{overlap} here~~
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
function (pattern, seq, overlap = TRUE) 
{
    if (!overlap) {
        nobs <- gregexpr(pattern, seq)[[1]]
        if (nobs[1] == -1) {
            return(0)
        }
        else return(length(nobs))
    }
    seq = seq
    k <- regexpr(pattern, seq)
    count = 0
    while (k[1] != -1) {
        seq <- substring(seq, k[1] + 1, nchar(seq))
        k <- regexpr(pattern, seq)
        count = count + 1
    }
    return(count)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
