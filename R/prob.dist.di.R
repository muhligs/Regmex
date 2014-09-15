prob.dist.di <-
function(pattern, seq, freq = FALSE, nt.null = 2, mode = FALSE, overlap = TRUE){ #include
  #if(freq[1]){print(freq);prb.init.st <- lambda.con(pattern,seq,freq=freq)[pattern$startState,]}#also include case nt.null=2 and freq!=FALSE
if(nt.null==1){prb.init.st <- lambda.con(pattern, seq, freq, overlap = overlap)[pattern$startState,]
  end.st <- pattern$endState
	}	else
  {prb.init.st <- lambda.con.di(pattern,seq,freq=freq, mode=mode, overlap = overlap)[1,-1]
   #tm<-transition.matrix.di(pattern$matrix,seq$freq.di)
  	end.st <- which(sub("[GCAT]","",rownames(pattern$matrix.di)) %in% as.character(pattern$endState))
  	#cat("endstate ",end.st,"\n")
  }
#	print(prb.init.st)
#	print(end.st)
  nop<-n.obs.pat(pattern$pattern,seq$seq, overlap = overlap)#this can be passed if optimized
	if (mode == "rw"){nop <- nop + 1}
  nop2 = nop
  if(nop==0){nop2=2}
  if(nop==1){nop2=2}
  
  if(nt.null==1) nSt <- dim(pattern$matrix)[1]
	if(nt.null==2) nSt<-dim(pattern$matrix.di)[1]
  #prb.dst <- rep(0,nop2)
	prb.dst <- rep(0,nop2+1)# we do this to have p(not observing) calculated (= prb.dst[1])
	#prb.dst[1]<-sum(prb.init.st[c(1:nSt)[-end.state]])
	#cat("p0",prb.init.st[c(1:nSt)[-end.state]],"\n")
     for (i in 1:nop2) {
       ### prb.dst[i] <- sum(prb.init.st[(nSt * (i - 1) + 1):(nSt * i)])
    	if(i == 1){prb.dst[i] <- sum(prb.init.st[1:nSt][-end.st]);next}###
    	prb.dst[i] <- sum(c(prb.init.st[(nSt * (i - 1) + 1):(nSt * i)][-end.st],prb.init.st[nSt * (i - 2)+end.st]))###
    }
		prb.dst[nop2+1] <- sum(prb.init.st[(nSt * (nop2-1)+end.st)])###
if (mode == "rw"){nop <- nop - 1}
pnom <- ifelse(mode=="rw",sum(prb.dst[c(nop+1,nop+2)]),prb.dst[nop+1])
    if (nop == 0) {
        return(list(prob.n.or.more = 1, n.obs.patterns = nop, 
            prob.dist = prb.dst, prob.1.or.more = 1 - prb.dst[1]))
    }
    if (nop == 1) {
        return(list(prob.n.or.more = 1 - prb.dst[1], n.obs.patterns = nop, 
            prob.dist = prb.dst, prob.1.or.more = 1 - prb.dst[1]))
    }
   return(list(prob.n.or.more = pnom, n.obs.patterns = nop, prob.dist = prb.dst, prob.1.or.more = 1 - prb.dst[1]))
 
#	for (i in 1:nop2+1){ (this is old stuf (obsolete) before rw and before fix of probability error.
    #prb.dst[i] <- sum(prb.init.st[(nSt*(i-1)+1):(nSt*i)])#does this not also count obs in endstate of previous block
 # 	prb.dst[i] <- sum(prb.init.st[c(c((nSt*(i-1)+1):(nSt*i))[-end.state],end.state+(nSt*(i-2)))],na.rm=TRUE)#
  #	cat(c(c((nSt*(i-1)+1):(nSt*i))[-end.state],end.state+(nSt*i)),"\n")
  #	cat(prb.init.st[c(c((nSt*(i-1)+1):(nSt*i))[-end.state],end.state+(nSt*(i-2)))],"\n")
  #	print((nSt*(i-1)+1):(nSt*i))
  #}
 # if(nop==0){return(list("prob.n.or.more"=1,"n.obs.patterns"=nop,"prob.dist"=prb.dst,"prob.no.obs"=prb.dst[1]))}
 # if(nop==1){return(list("prob.n.or.more"=prb.init.st[end.state],"n.obs.patterns"=nop,"prob.dist"=prb.dst,"prob.no.obs"=prb.dst[1]))}
 # return(list("prob.n.or.more"=prb.dst[nop+1],"n.obs.patterns"=nop,"prob.dist"=prb.dst,"prob.no.obs"=prb.dst[1]))
}
