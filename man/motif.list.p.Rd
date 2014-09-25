\name{motif.list.p}
\alias{motif.list.p}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{a function to calculate rank correlation p-values for a set of motifs.
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
motif.list.p(seqlist, motiflist, cores = 1, mode = "bb", order = 1, exact = TRUE, overlap = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{seqlist}{
A list of ranked sequences in which the motif rank correlation is calculated. 
The sequence list should be a list object containing 'seq' objects or character strings.
}
  \item{motiflist}{
A list of motifs (string objects)
}
  \item{cores}{
Number of cores to use in parallel.
}
  \item{mode}{
One of "bb", "rw" or "msr" depending on the correlation evaluation method.
}
  \item{order}{
1 or 2 for mono-nucleotide or di-nucleotide dependency of sequence specific p-value calculation.
}
  \item{exact}{
%%     ~~Describe \code{exact} here~~
}
  \item{overlap}{
Logical, are motifs allowed to overlap in the sequences.
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
function (seqlist, motiflist, cores = 1, mode = "bb", order = 1, 
    exact = TRUE, overlap = TRUE) 
{
    ptm <- proc.time()
    if (!(mode \%in\% c("bb", "mw", "rw", "mhg"))) {
        stop("mode should be one of 'bb'(default), 'mw' 'rw' or 'mhg'")
    }
    if (mode == "mhg") {
        length.ml <- length(motiflist)
        cat("preparing ", length.ml, " motifs", "\n")
        cat(paste("start ", Sys.time()), "\n")
        if (is.list(motiflist)) 
            motiflist <- unlist(mclapply(motiflist, function(x) x$pattern, 
                mc.cores = cores))
        cat(paste("\n", "end ", Sys.time()), "\n\n")
        cat("preparing ", length(seqlist), " sequences", "\n")
        if (is.list(seqlist)) 
            seqlist <- unlist(mclapply(seqlist, function(x) x$sequence, 
                mc.cores = cores))
        cat(paste("\n", "end ", Sys.time()), "\n\n")
        cat("calculating probabilities...", "\n")
        cat(paste("start ", Sys.time()), "\n")
        cat("progress... ")
        vec2 <- rep(NA, length.ml)
        idx <- 0
        end <- 0
        if (length.ml < 100) {
            counter <- c(30, 60, 100)
        }
        else counter <- c(1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 
            80, 90, 100)
        for (i in counter) {
            idx <- idx + 1
            start <- end + 1
            end <- round(i/100 * length.ml, 0)
            vec <- unlist(mclapply(motiflist[start:end], function(x) mhg.motif.p(seqlist = seqlist, 
                motif = x, ...), mc.cores = cores))
            cat(i, "\%.. ", sep = "")
            vec2[start:end] <- vec
        }
        names(vec2) <- motiflist
        cat(paste("\n", "finished ", Sys.time()), "  ,  total time (s): ", 
            proc.time()[3] - ptm[3], "\n")
        return(vec2)
    }
    if (class(seqlist[[1]]) == "character") {
        cat("preparing sequences", "\n")
        seqlist <- seq.list.con(seqlist, cores = cores)
    }
    if (class(seqlist[[1]]) != "seq") {
        stop("sequences should be character vector or 'seq' object")
    }
    length.ml <- length(motiflist)
    if (class(motiflist) == "character") {
        cat("preparing ", length.ml, " motifs", "\n")
        cat(paste("start ", Sys.time()), "\n")
        cat("progress... ")
        motiflist3 <- list()
        idx <- 0
        end <- 0
        if (length.ml < 100) {
            counter <- c(30, 60, 100)
        }
        else counter <- c(1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 
            80, 90, 100)
        for (i in counter) {
            idx <- idx + 1
            start <- end + 1
            end <- round(i/100 * length.ml, 0)
            cat(i, "\%.. ", sep = "")
            motiflist2 <- mclapply(motiflist[start:end], function(motif) pat.con(motif), 
                mc.cores = cores)
            motiflist3 <- append(motiflist3, motiflist2)
        }
    }
    if (class(motiflist[[1]]) == "pattern") {
        motiflist3 <- motiflist
        motiflist <- unlist(mclapply(motiflist3, function(x) x$pattern, 
            mc.cores = cores))
    }
    cat(paste("\n", "end ", Sys.time()), "\n\n")
    if (class(motiflist3[[1]]) != "pattern") {
        stop("motif should be character vector or list of 'pattern' objects")
    }
    cat("calculating probabilities...", "\n")
    cat(paste("start ", Sys.time()), "\n")
    cat("progress... ")
    vec2 <- rep(NA, length.ml)
    end <- 0
    if (length.ml < 100 & length.ml > cores) {
        counter <- c(30, 60, 100)
    }
    if (length.ml > 100) {
        counter <- c(1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 
            90, 100)
    }
    if (length.ml < (cores + 1)) {
        counter <- 100
    }
    for (i in counter) {
        start <- end + 1
        end <- round(i/100 * length.ml, 0)
        vec <- unlist(mclapply(motiflist3[start:end], function(x) motif.p(seqlist, 
            x, mode = mode, order = order, exact = exact, overlap = overlap), 
            mc.cores = cores))
        cat(i, "\%.. ", sep = "")
        vec2[start:end] <- vec
    }
    names(vec2) <- motiflist
    cat(paste("\n", "finished ", Sys.time()), "  ,  total time (s): ", 
        proc.time()[3] - ptm[3], "\n")
    return(vec2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
