par.list.prob <-
function(seq.list,pattern,cores=detectCores(), overlap = TRUE){ #include
  require(parallel)
  fun<-function(x){return(prob.dist(pattern,x, overlap = overlap)$prob.n.or.more)}
  return(unlist(mclapply(seq.list, fun, mc.cores=cores)))
}
