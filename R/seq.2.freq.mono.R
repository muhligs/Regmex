seq.2.freq.mono <-
function(seq){ # include
mononuc.names<-c("A","C","G","T")
n<-nchar(seq)
return(table(factor(substring(seq,1:n,1:n),levels=mononuc.names))/n)
}
