\name{l.clust}
\alias{l.clust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{a function to cluster a set of motifs by levenshtein distances.
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
l.clust(set, k, plot = TRUE, method = "average")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{set}{
%%     ~~Describe \code{set} here~~
}
  \item{k}{
%%     ~~Describe \code{k} here~~
}
  \item{plot}{
%%     ~~Describe \code{plot} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
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
function (set, k, plot = TRUE, method = "average") 
{
    d <- adist(set)
    rownames(d) <- set
    hc <- hclust(as.dist(d), method = method)
    c.ex <- ifelse(length(set) < 70, 30/length(set), 40/length(set))
    if (plot == TRUE) 
        plot(hc, xlab = "motifs", cex = c.ex)
    rect.hclust(hc, k = k)
    return(data.frame(cutree(hc, k = k)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
