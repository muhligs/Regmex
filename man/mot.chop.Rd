\name{mot.chop}
\alias{mot.chop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{function to cut a sequence into all the possible n mers it consist of.
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mot.chop(seq, mers, unique = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{seq}{
%%     ~~Describe \code{seq} here~~
}
  \item{mers}{
%%     ~~Describe \code{mers} here~~
}
  \item{unique}{
%%     ~~Describe \code{unique} here~~
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
function (seq, mers, unique = TRUE) 
{
    starts <- 1:(nchar(seq) - mers + 1)
    ends <- mers:nchar(seq)
    return(if (unique) unique(substring(seq, starts, ends)) else substring(seq, 
        starts, ends))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
