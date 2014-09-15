rw.find.n.exc <-
function(score.scheme,len)#include
{
# Mean step size
step.size <- sum(as.numeric(names(score.scheme))*score.scheme)
# Mean recursion length
	A <- 1/(-step.size)
# return expected number of excursions
	return(floor(len/A))
}
