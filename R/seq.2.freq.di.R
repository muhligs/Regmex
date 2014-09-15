seq.2.freq.di <-
function(seq,pseudo=0){ # include
dinuc.names<-c("AA","AC","AG","AT","CA","CC","CG","CT","GA","GC","GG","GT","TA","TC","TG","TT")
n<-nchar(seq)
freq.tab<-table(factor(substring(seq,1:(n-2+1),2:n),levels=dinuc.names))+pseudo
return(freq.tab/sum(freq.tab))
}
