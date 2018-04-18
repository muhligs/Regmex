motif.p <-
function(seqlist, motif, overlap=TRUE, mode="bb",cores=1, sub.method="p.value", order=1, exact=TRUE, alpha=0.00001,...){ # include
	#if("flag.motif.list.p" %in% ls(envir = sys.frame(-1))){} else {
if	(!(mode %in% c("bb","msr","rw"))){stop("mode should be one of 'bb'(default), 'msr' or 'rw'")}
if(mode == "mhg"){# in dev.
	if(is.list(motif)) motif <- motif$pattern
	if(is.list(seqlist)) seqlist	<- unlist(mclapply(seqlist,function(x)x$sequence, mc.cores = cores))
	res <- mhg.motif.p(seqlist = seqlist, motif = motif, ...)
	names(res) <- motif
return(res)	
	}	

if(class(seqlist[[1]])=="character"){seqlist<-seq.list.con(seqlist,cores=cores)}
if(class(seqlist[[1]])!="seq"){stop("sequences should be character vector or 'seq' object")}  
if	(class(motif)=="character"){motif<-pat.con(motif)}
if	(class(motif)!="pattern"){stop("motif should be character vector or 'pattern' object")}
#	}
# calculate sequence p values
## 	for Brownian Bridge or Ranked Sum
if(mode %in% c("rs","bb")){if(order==1) {p.values<-par.list.prob(seqlist, motif, cores=cores, overlap = overlap, ...)} else
    {if(order==2)p.values<-unlist(mclapply(seqlist,function(x){prob.dist.di(motif, x, mode=mode, overlap = overlap, ...)$prob.n.or.more},mc.cores=cores))}} else
## modified Wilcoxon RS test
if(mode=="msr"){if(order==1) {values<-mclapply(seqlist,function(x){prob.dist(motif,x)},mc.cores=cores)} else
    {if(order==2){values<-mclapply(seqlist,function(x){prob.dist.di(motif,x)},mc.cores=cores)}}
### adjust so that p(1orMore) is calculated from prob.dist.di
p.values<-sapply(values,function(x)return(x$prob.1.or.more))
n.obs<-sapply(values,function(x)return(x$n.obs.patterns))
}
## Random Walk
	if(mode == "rw"){
	if(order == 1) {p.values <- unlist(mclapply(seqlist,function(x){rw.draw.p(prob.dist(motif,x,mode=mode, overlap = overlap, ...))},mc.cores=cores))} else
    {if(order==2){p.values <- unlist(mclapply(seqlist,function(x){rw.draw.p(prob.dist.di(motif,x,mode=mode, overlap = overlap, ...))},mc.cores=cores))}}
	 }
#print(p.values)
# calculate motif pvalue
	if(mode=="rw"){if(sub.method=="p.value") return(rw.motif.p(p.values, ...)$p.value) else return(rw.motif.p(p.values, ...))}
	if(mode=="bb"){if(sub.method=="p.value") return(bb.motif.p(p.values,exact=exact, alpha=alpha)$p.value) else return(bb.motif.p(p.values,exact=exact, alpha=alpha))}
	if(mode=="rs"){if(sub.method=="p.value") return(rs.motif.p(p.values)) else return(rs.motif.p(p.values, rs.out="all"))}
	if(mode=="msr"){return(wcmod.p(p.values,n.obs))}
}
