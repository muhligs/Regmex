rev.comp <-
	function(seqs){
res <- sapply(seqs,function(x)paste(chartr("CGAT","GCTA",rev(unlist(strsplit(x,"")))),collapse=""))
		names(res) <- NULL
		return(res)
	}
