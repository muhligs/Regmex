seq.con <-
function(seq,pseudo=0){ # include
	freq.di <-seq.2.freq.di(seq,pseudo)
   lst<-list(sequence=seq,freq.mono=seq.2.freq.mono(seq),freq.di=freq.di,con.prob.di=con.prob.di(freq.di),length=nchar(seq))
   class(lst)<-"seq"
  return(lst)
}
