transition.matrix.di <-
function(di.dfa,con.prob){ #include
  n.states<-dim(di.dfa)[1]
	states <- rownames(di.dfa)
  tr.mat <- matrix(0,nrow=n.states,ncol=n.states)
	 rownames(tr.mat)<-states
	 colnames(tr.mat)<-states
 	givenbase <- 	gsub("[0123456789]","",states)
  for (i in 1:n.states){#for (i in 1:n.states){
    for (j in colnames(di.dfa)){
      to.st <- di.dfa[states[i],j]
      tr.mat[states[i],to.st] <- tr.mat[states[i],to.st]+con.prob[j,givenbase[i]]
    }
  }
  return(tr.mat)
}
