motifs.seqs <-
function(seqs,k){ # include
if(class(seqs)=="seq"){seqs <- seqs$sequence}
if(class(seqs)=="list"){
  if(!(class(seqs[[1]]) %in% c("character","seq"))){stop("seq should be a 'seq' object or a string or a list of 'seq' objects or strings")}
 	if(class(seqs[[1]])=="seq"){seqs <- lapply(seqs,function(x)x$sequence)}
}
vec <- c()
for(i in seqs) {vec <- c(vec,mot.chop(i,k,unique=TRUE))}
return(unique(vec))
}
