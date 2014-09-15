rw.max.p <-
function(scoring.scheme, wmax, nseqs, n.exc=1){#include
tr.mat <- rw.transition.matrix(scoring.scheme, wmax)
  var <- (tr.mat %^% nseqs)[2,wmax+2]
	return(rw.max.dist(var, n.exc))
#	return(list(a=(1-var)^nseqs,b=1-(1-var)^nseqs,c=var))
}
