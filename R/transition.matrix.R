transition.matrix <-
function(pm,nt.freq){ #include
  n.states<-dim(pm)[1]
  tr.mat <- matrix(0,nrow=n.states,ncol=n.states)
	 rownames(tr.mat)<-rownames(pm)
	 colnames(tr.mat)<-rownames(pm)
  for (i in 1:n.states){
    for (j in as.character(names(nt.freq))){
      to.st <- pm[i,j]
      tr.mat[i,to.st] <- tr.mat[i,to.st]+nt.freq[j]
    }
  }
  return(tr.mat)
}
