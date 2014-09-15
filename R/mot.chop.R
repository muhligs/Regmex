mot.chop <-
function(seq,mers,unique=TRUE){ #include
starts <- 1:(nchar(seq)-mers+1)
ends <- mers:nchar(seq)
return(if(unique) unique(substring(seq,starts,ends)) else substring(seq,starts,ends))
}
