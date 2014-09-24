l.clust <-
function(set,k,plot=TRUE,method="average"){ # include
d  <- adist(set)
	rownames(d) <- set
	print(class(dist))
hc <- hclust(as.dist(d),method=method)
	c.ex <- ifelse(length(set)<70,30/length(set),40/length(set))
if(plot==TRUE)	plot(hc,xlab = "motifs",cex=c.ex)
rect.hclust(hc,k=k)
res <- 	data.frame(cutree(hc,k=k))
res$motif <- as.character(set)
colnames(res)[1] <- "cluster"
return(res)
}
