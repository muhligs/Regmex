list.prob <-
function(seq.list,pattern,...){ #include
  fun<-function(x){return(prob.dist(pattern,x,...)$prob.n.or.more)}
  return(unlist(lapply(seq.list,fun)))
}
