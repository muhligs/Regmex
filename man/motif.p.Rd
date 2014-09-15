\name{motif.p}
\alias{motif.p}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{a function to calculate a rank correlation p-value for a single motif defined as a regular expression.
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
motif.p(seqlist, motif, overlap = TRUE, mode = "bb", cores = 1, sub.method = "p.value", order = 1, exact = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{seqlist}{
%%     ~~Describe \code{seqlist} here~~
}
  \item{motif}{
%%     ~~Describe \code{motif} here~~
}
  \item{overlap}{
%%     ~~Describe \code{overlap} here~~
}
  \item{mode}{
%%     ~~Describe \code{mode} here~~
}
  \item{cores}{
%%     ~~Describe \code{cores} here~~
}
  \item{sub.method}{
%%     ~~Describe \code{sub.method} here~~
}
  \item{order}{
%%     ~~Describe \code{order} here~~
}
  \item{exact}{
%%     ~~Describe \code{exact} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (seqlist, motif, overlap = TRUE, mode = "bb", cores = 1, 
    sub.method = "p.value", order = 1, exact = TRUE, ...) 
{
    if (!(mode \%in\% c("bb", "mw", "rw", "mhg"))) {
        stop("mode should be one of 'bb'(default), 'mw' 'rw' or 'mhg'")
    }
    if (mode == "mhg") {
        if (is.list(motif)) 
            motif <- motif$pattern
        if (is.list(seqlist)) 
            seqlist <- unlist(mclapply(seqlist, function(x) x$sequence, 
                mc.cores = cores))
        res <- mhg.motif.p(seqlist = seqlist, motif = motif, 
            ...)
        names(res) <- motif
        return(res)
    }
    if (class(seqlist[[1]]) == "character") {
        seqlist <- seq.list.con(seqlist, cores = cores)
    }
    if (class(seqlist[[1]]) != "seq") {
        stop("sequences should be character vector or 'seq' object")
    }
    if (class(motif) == "character") {
        motif <- pat.con(motif)
    }
    if (class(motif) != "pattern") {
        stop("motif should be character vector or 'pattern' object")
    }
    if (mode \%in\% c("rs", "bb")) {
        if (order == 1) {
            p.values <- par.list.prob(seqlist, motif, cores = cores, 
                overlap = overlap, ...)
        }
        else {
            if (order == 2) 
                p.values <- unlist(mclapply(seqlist, function(x) {
                  prob.dist.di(motif, x, mode = mode, overlap = overlap, 
                    ...)$prob.n.or.more
                }, mc.cores = cores))
        }
    }
    else if (mode == "mw") {
        if (order == 1) {
            values <- mclapply(seqlist, function(x) {
                prob.dist(motif, x)
            }, mc.cores = cores)
        }
        else {
            if (order == 2) {
                values <- mclapply(seqlist, function(x) {
                  prob.dist.di(motif, x)
                }, mc.cores = cores)
            }
        }
        p.values <- sapply(values, function(x) return(x$prob.1.or.more))
        n.obs <- sapply(values, function(x) return(x$n.obs.patterns))
    }
    if (mode == "rw") {
        if (order == 1) {
            p.values <- unlist(mclapply(seqlist, function(x) {
                rw.draw.p(prob.dist(motif, x, mode = mode, overlap = overlap, 
                  ...))
            }, mc.cores = cores))
        }
        else {
            if (order == 2) {
                p.values <- unlist(mclapply(seqlist, function(x) {
                  rw.draw.p(prob.dist.di(motif, x, mode = mode, 
                    overlap = overlap, ...))
                }, mc.cores = cores))
            }
        }
    }
    if (mode == "rw") {
        if (sub.method == "p.value") 
            return(rw.motif.p(p.values, ...)$p.value)
        else return(rw.motif.p(p.values, ...))
    }
    if (mode == "bb") {
        if (sub.method == "p.value") 
            return(bb.motif.p(p.values, exact = exact)$p.value)
        else return(bb.motif.p(p.values, exact = exact))
    }
    if (mode == "rs") {
        if (sub.method == "p.value") 
            return(rs.motif.p(p.values))
        else return(rs.motif.p(p.values, rs.out = "all"))
    }
    if (mode == "mw") {
        return(wcmod.p(p.values, n.obs))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
