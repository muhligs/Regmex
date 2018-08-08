lambda.con <-
function(pattern,seq,freq=FALSE,nt.null=1,mode=FALSE, overlap = TRUE){ #include
  ## Construct Lambda matrix (blocks of trmatrix)

# make transition probability matrix from dfa and base frequencies.  
# check for input frequencies or use sequence frequencies
   if(freq){tm<-transition.matrix(pattern$matrix,freq)} else
   {tm<-transition.matrix(pattern$matrix,seq$freq.mono)}
  
# number of observed patterns  
  nop<-n.obs.pat(pattern$pattern,seq$seq, overlap = overlap)
  if(mode == "rw"){nop <- nop + 1}
  nop<-ifelse(nop %in% c(0,1),2,nop)#made to handle case nop = 0

  finl.st<-pattern$endState # final state
  n.states<-dim(tm)[1] # number of states in dfa

  # produce the embedded dfa called lam(bda) # remarks display example with n.states = 5 and nop = 3
  lam <- matrix(0,n.states*nop,n.states*nop) # empty matrix with embedded dimension 15x15.
  for (i in 1:nop){ # 1,2,3
    bgn.indx <- (i-1)*n.states+1 # 1,6,11
    end.indx <- (i*n.states) # 5,10,15
    blck.indx <- bgn.indx:end.indx # 1:5,6:10,11:15
    lam[blck.indx,blck.indx] <- tm # fill out lambda with the transition prob matrix.
  }
  # Final states should be shifted forward in rows, so that probabilities for entering a final states becomes the probability of entering the next start state.
  for (k in finl.st){ # there is usually only one, but there could be more.
    for (i in 1:(nop-1)){# for each pattern observation but the last. Substitutes below obsolete line. 1,2
    #  for (i in 1:ifelse(nop==1,1,(nop-1))){# instead of : for (i in 1:(nop-1)){ , to handle case nop = 0 or 1)
      ## Row(s) that should be shifted
      bgn.finl <- (i-1)*n.states+k # finl.st,5+finl.st
      ## Block as before
      bgn.indx <- (i-1)*n.states+1 # 1,6
      end.indx <- (i*n.states) # 5,10
      blck.indx <- bgn.indx:end.indx # 1:5,6:10
      ## Shift
      #if(nop>1){lam[bgn.finl,blck.indx+n.states] <- lam[bgn.finl,blck.indx]}#only shift if nop > 1
      lam[bgn.finl,blck.indx+n.states] <- lam[bgn.finl,blck.indx] # nop is always bigger than one.
      lam[bgn.finl,blck.indx] <- 0
    }
  }
  ## In final ending states we should stay
  end.st <- n.states*(nop-1)+finl.st # 5*2+finl.st
  for (i in end.st){ # again, there could be more.
     lam[i,] <- 0 # no way out of this state...
     lam[i,i] <- 1 # except to the same state
  }
  return(lam%^%seq$length) # return the eTPM lifted to the power of the sequence length.  
}
