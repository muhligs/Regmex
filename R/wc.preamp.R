wc.preamp <-
function(seqlist,motif,cores=1){ #include
if(class(seqlist)=="list") seqs <- unlist(lapply(seqlist,function(x)x$sequence)) else seqs <- seqlist
var2 <- unlist(mclapply(motif,function(x){var <- grepl(x,seqs);if(sum(var) %in% c(0,length(seqs))){return(1)};return(wilcox.test(which(var),which(!var))$p.value)},mc.cores=cores))
	names(var2) <- motif
return(var2)
}
