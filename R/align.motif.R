align.motif <-
function(motif,seqlist,lf,rf){ # include
	vec.res<-c()
for(i in seqlist){
vec <- gregexpr(motif,i$sequence)[[1]]
	if(vec[1]==-1)next
	for(k in vec){
#	cat(substr(i$sequence,k-lf,k+nchar(motif)+rf-1),"\n")
vec.res <- c(vec.res,substr(i$sequence,k-lf,k+nchar(motif)+rf-1))
	}}
	return(vec.res)
	}
