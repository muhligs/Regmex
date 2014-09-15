bb.motif.p <-
function(p.values,alpha=0.00001,exact=TRUE,dist=NULL){ #include
if(sum(p.values!=1)==0){return(list("p.value"=1,"z"=0,"rs"=rep(0,length(p.values)+1), "pvals"=p.values))}
	ls=-log(ifelse(p.values+alpha<1,p.values+alpha,p.values))
lmean<-mean(ls)
#define a running sum of the log score: r(i)=r(i-1) + ls[i-1]-lmean
lp <- length(p.values)
rs=rep(0,lp+1)
for(i in 2:(lp+1)){
rs[i]=rs[i-1]+ls[i-1]-lmean #
}
	#alternative: (there is a bug in this:)

#	cs <- cumsum(ls)
#	rs <- c(cs-ls[1]-cs[lp]*(0:(lp-1))/lp,0)
# get max
#print(rs)
#	print(lmean)
maxR=max(abs(rs))
if(exact==FALSE){
	# sd of ls
	sdls<-sd(ls)
#get mean of maxR null distribution
mean.nd<-sqrt(pi*length(p.values)/2)*log(2)
# get variance of maxR null distribution
var.nd<-(length(p.values)*pi^2)/12-mean.nd^2
#calculate z score
	maxR.z<-(maxR/sdls-mean.nd)/sqrt(var.nd)
# calculate p.value
	p.value<-2*pnorm(-abs(maxR.z))
}
	if(exact==TRUE){
	if(is.null(dist)){
		if(exp(-2/lp*maxR^2) < 2e-16){p.value <- 2*exp(-2/lp*maxR^2)} else p.value <- 1-bb.cumdist(maxR,lp)
		# this is from a bernoulli inequality to avoid a problem with (1-x)^n being truncated for x near 1 and large n.
	} else {p.value <- 1-dist[[2]][which(abs(dist[[1]]-maxR)==min(abs(dist[[1]]-maxR)))] }
#	fm <- 8*(-1^(0:99))*((1:100)^2)*maxR*exp(-2*((1:100)^2)*maxR^2)
	}
return(list("p.value"=p.value,"maxR"=maxR,"rs"=rs, "pvals"=p.values))
#	return(p.value)#"z"=maxR.z,"rs"=rs, "pvals"=p.values))
}
