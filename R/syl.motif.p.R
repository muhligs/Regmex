syl.motif.p <-
function(seqlist, motifs, bins = 40, cores = 1, all.p = FALSE, verbose=FALSE){ # include
ptm <- proc.time()
bin.idx <- c(seq(1,length(seqlist),by = bins),length(seqlist)+1)
if(!verbose)	{cat("finding motifs","\n");	cat(paste("start ",Sys.time()),"\n")}
	res <- matrix(0,ncol=length(bin.idx)-1,nrow=length(motifs))
	total.count <- unlist(mclapply(motifs,function(x)sum(unlist(gregexpr(x,seqlist))>-1),mc.cores=cores))
	bg.total.count <- sum(unlist(lapply(seqlist,nchar)))
	bin1 <- 0
bin.balls <- 0
counter <-	round(quantile(1:length(bin.idx),probs=1:100/100))[c(1,2,5,seq(10,100,10))]
if(!verbose)	cat("calculating bin p.values","\n")
	for(i in 2:length(bin.idx)){
bin.count <-	unlist(mclapply(motifs,function(x)sum(unlist(gregexpr(x,seqlist[bin.idx[i-1]:(bin.idx[i]-1)]))>-1),mc.cores=cores))
		balls <- sum(unlist(lapply(seqlist[bin.idx[i-1]:(bin.idx[i]-1)],nchar)))
		bin.balls <- bin.balls+balls
		bin1 <- bin1 + bin.count
#res[,i-1] <- 	phyper(q = bin1-1, m = total.count, n = length(seqlist)-total.count, k = (bin.idx[i]-1), lower.tail = FALSE)
		res[,i-1] <- 	phyper(q = bin1-1, m = total.count, n = bg.total.count-total.count, k = bin.balls, lower.tail = FALSE)
		if(!verbose)if(i %in% counter)cat(names(counter)[which(counter==i)],".")
}
if(!verbose)		cat(paste("finished at ",Sys.time()),"  total time (s): ",proc.time()[3]-ptm[3],"\n")
	if(all.p){rownames(res) <- motifs
		colnames(res) <- (bin.idx-1)[-1]
		return(res)
	}
	res2 <- apply(res,1,min)
	names(res2) <- motifs
	return(res2)
}
