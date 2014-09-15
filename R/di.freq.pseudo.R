di.freq.pseudo <-
function(seqlist,pseudocounts=16){ # include
pseudocounts*unlist(apply(as.data.frame(lapply(seqlist,function(seq)as.vector(seq$freq.di*seq$length))),1,sum))/sum(unlist(lapply(seqlist,function(y)y$length)))
}
