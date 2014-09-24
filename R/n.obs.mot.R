n.obs.mot <-
function(motif,seq,overlap=TRUE){ #include
if(class(seq)=="character"){return(n.obs.pat(motif,seq,overlap=overlap))}
if(class(seq)=="seq"){return(n.obs.pat(motif,seq$sequence,overlap=overlap))}
if(class(seq)=="list"){
	if(!(class(seq[[1]]) %in% c("character","seq"))){stop("seq should be a 'seq' object or a string or a list of 'seq' objects or strings")}
  if(class(seq[[1]])=="character"){res <- unlist(lapply(seq,function(x)n.obs.pat(motif,x,overlap=overlap)))
  return(res)}
	if(class(seq[[1]])=="seq"){res <- unlist(lapply(seq,function(x)n.obs.pat(motif,x$sequence,overlap=overlap)))
  return(res)}
}
stop("seq should be a 'seq' object or a string or a list of 'seq' objects or strings")
}
