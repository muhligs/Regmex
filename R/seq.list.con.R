seq.list.con <-
function(seqlist,cores=1){ #include
seqlist <- mclapply(seqlist,seq.con,mc.cores=cores)
pseudo <- di.freq.pseudo(seqlist)
return(mclapply(seqlist,function(seq)seq.con(seq$sequence,pseudo),mc.cores=cores))
}
