motifs.seqs <-
function(seqs,k){ # include
vec <- c()
for(i in seqs)vec <- c(vec,mot.chop(i,k,unique=TRUE))
return(unique(vec))
}
