n.obs.pat <-
function(pattern,seq,overlap=TRUE){ #include
	if(!overlap){
		nobs <- gregexpr(pattern,seq)[[1]]
		if(nobs[1] == -1){return(0)} else 
			return(length(nobs))
	}
  seq=seq
  k<-regexpr(pattern,seq)
  count=0
  while(k[1]!=-1){
    seq<-substring(seq,k[1]+1,nchar(seq))
    #print(seq)
    k<-regexpr(pattern,seq)
    count=count+1
  }
  return(count)
}
