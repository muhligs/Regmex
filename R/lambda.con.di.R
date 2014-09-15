lambda.con.di <-
function(pattern, seq, freq = FALSE, nt.null = 2, mode = FALSE, overlap = TRUE){ #include
  ## Construct Lambda matrix (blocks of trmatrix)
  #if(freq){tm<-transition.matrix.di(pattern$matrix,freq)} else
  #{tm<-transition.matrix.di(pattern$matrix,seq$freq.di)}
  if(freq){tm <- transition.matrix.di(pattern$matrix, freq)} else
  { tm <- transition.matrix.di(pattern$matrix.di, seq$con.prob.di) }
	 #if(freq==FALSE)freq=seq$freq.di
  #tm<-transition.matrix.di(pattern$matrix,freq)
	#cat("dim tm: ",dim(tm),"\n")
	#print(class(tm))
	 # get number of observations
  nop<-n.obs.pat(pattern$pattern,seq$seq, overlap = overlap)
  if(mode == "rw"){nop <- nop + 1}
  nop<-ifelse(nop %in% c(0,1),2,nop)#made to handle case nop = 0
  # we need to get the start states:
 	start.state.mono<-pattern$startState
	# the start states  of the di.state model are the set of "to" states from the old start state combined with the bases that point to them.
	# or, the start states  of the di.state model are the set of di.states that hvae mono.start state as their first name
	#start.states.di<-paste(pattern$matrix[start.state.mono,],colnames(pattern$matrix),sep="")
	#or
#	start.states.di<-which(colnames(tm) %in% colnames(tm)[sub("[ACGT]","",colnames(tm))==start.state.mono])
#		start.states.di<-which(colnames(tm) %in% colnames(tm)[sub("[ACGT]","",colnames(tm))==as.character(start.state.mono)])
	start.states.di<-which(sub("[ACGT]","",colnames(tm)) %in% as.character(start.state.mono))
	# mono end state:
	#print(start.state.mono)
 #print(start.states.di)
	#print(colnames(tm)[start.states.di])
	finl.st<-pattern$endState
	# di.nt end states are the set of di.states that have mono.end state(s) as their first name (e.g. 1C and 1A if mono.end state was 1)
	#finl.states.di.idx<-which(sub("[ACGT]","",colnames(tm))==finl.st)
	finl.states.di.idx<-which(sub("[ACGT]","",colnames(tm)) %in% as.character(finl.st))
	#print(tm)

	#print(colnames(tm))
	#print(finl.st)
	#print(finl.states.di.idx)
	#print(colnames(tm)[finl.states.di.idx])
  n.states<-dim(tm)[1]
  lam <- matrix(0,n.states*nop,n.states*nop)
	#cat("dim lam1=",dim(lam),"\n")
 #fill out the basics of the embedded model matrix 
#print(class(lam))
	for (i in 1:nop){
    bgn.indx <- (i-1)*n.states+1
    end.indx <- (i*n.states)
    blck.indx <- bgn.indx:end.indx 
		#cat("blck.indx=",blck.indx,"\n")
		#print(lam[blck.indx,blck.indx])
		
    lam[blck.indx,blck.indx] <- tm
		#print(class(lam))
  }
	 #cat("dim lam2=",dim(lam),"\n")
		#print(lam)
  # Final states should be shifted forward in rows
  for (k in finl.states.di.idx){# for (k in finl.st){ in the momo case
    for (i in 1:ifelse(nop==1,1,(nop-1))){# instead of : for (i in 1:(nop-1)){ , to handle case nop = 0 or 1)
      ## Row(s) that should be shifted
      bgn.finl <- (i-1)*n.states+k #;cat("bgnfinl=",bgn.finl,"\n")
      ## Block as before
      bgn.indx <- (i-1)*n.states+1
      end.indx <- (i*n.states)
      blck.indx <- bgn.indx:end.indx #;cat("blck.indx=",blck.indx,"\n")
      ## Shift
      if(nop>1){lam[bgn.finl,blck.indx+n.states] <- lam[bgn.finl,blck.indx]}#only shift if nop > 1
      lam[bgn.finl,blck.indx] <- 0
    }
   }
  ## In final ending states we should stay
  end.st <- n.states*(nop-1)+finl.states.di.idx # momo case : end.st <- n.states*(nop-1)+finl.st
  for (i in end.st){
     lam[i,] <- 0
     lam[i,i] <- 1
     }
	#make "artificial" intitial states
	lam<-rbind(0,cbind(0,lam))
#	first.steps<-which(colnames(tm) %in% paste(pattern$matrix[pattern$startState,],colnames(pattern$matrix),sep=""))
		#first.steps<-which(colnames(tm) %in% paste(pattern$startState,colnames(pattern$matrix),sep=""))
	first.steps<-which(gsub("[GCAT]","",colnames(tm)) %in% pattern$startState)
	lam[1,first.steps+1] <- unlist(lapply(gsub("[[:digit:]]","",colnames(tm)[first.steps]),function(x){seq$freq.mono[x]}))
	#lam[1,start.states.di+1] <- 1/3
  #lam[end.st,end.st] <- 1
   #print(sum(lam))
#	print(lam)
  return(lam%^%seq$length)  
}
