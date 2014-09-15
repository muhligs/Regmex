lambda.con <-
function(pattern,seq,freq=FALSE,nt.null=1,mode=FALSE, overlap = TRUE){ #include
  ## Construct Lambda matrix (blocks of trmatrix)
   if(freq){tm<-transition.matrix(pattern$matrix,freq)} else
  {tm<-transition.matrix(pattern$matrix,seq$freq.mono)}
  nop<-n.obs.pat(pattern$pattern,seq$seq, overlap = overlap)
  if(mode == "rw"){nop <- nop + 1}
  nop<-ifelse(nop %in% c(0,1),2,nop)#made to handle case nop = 0
  #if(nop%in%c(0,1)){nop<-2}#not needed now, solution above better.
  finl.st<-pattern$endState
  n.states<-dim(tm)[1]
  lam <- matrix(0,n.states*nop,n.states*nop)
  for (i in 1:nop){
    bgn.indx <- (i-1)*n.states+1
    end.indx <- (i*n.states)
    blck.indx <- bgn.indx:end.indx
    lam[blck.indx,blck.indx] <- tm
  }
  ## Final states should be shifted forward in rows
  for (k in finl.st){
    for (i in 1:ifelse(nop==1,1,(nop-1))){# instead of : for (i in 1:(nop-1)){ , to handle case nop = 0 or 1)
      ## Row(s) that should be shifted
      bgn.finl <- (i-1)*n.states+k
      ## Block as before
      bgn.indx <- (i-1)*n.states+1
      end.indx <- (i*n.states)
      blck.indx <- bgn.indx:end.indx
      ## Shift
      if(nop>1){lam[bgn.finl,blck.indx+n.states] <- lam[bgn.finl,blck.indx]}#only shift if nop > 1
      lam[bgn.finl,blck.indx] <- 0
    }
   }
  ## In final ending states we should stay
  end.st <- n.states*(nop-1)+finl.st
  for (i in end.st){
     lam[i,] <- 0
     lam[i,i] <- 1
     }
  #lam[end.st,] <- 0
  #lam[end.st,end.st] <- 1
   #print(sum(lam))
  return(lam%^%seq$length)  
}
