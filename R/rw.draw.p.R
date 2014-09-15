rw.draw.p <-
function(pdist)#include
{
	if(pdist$n.obs.patterns==0){return(runif(1,pdist$prob.1.or.more,1))}
	vec <- pdist$prob.dist
	n <- length(vec)
return(runif(1,vec[n],sum(vec[c(n-1,n)])))
}
