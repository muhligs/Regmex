rw.motif.p <-
function (p.values, sc.par = 0.2) #include
{
 #score.scheme <- rw.score.scheme(rw.find.alpha(p.values))
	n.seqs <- length(p.values)
	#if(sum(p.values==1) > n.seqs/2){score.scheme <- rw.score.scheme2(p.values)} else { obsolete after the draw procedure
#	if(!alpha){alpha <- rw.find.alpha(p.values)} obsolete after the draw procedure
	#if(sc.par == "ln"){
	#score.scheme <- rw.score.scheme(alpha)
	#scores <- score.scheme$score.function(p.values) } else {
	score.scheme <- rw.ss(sc.par)
	scores <- score.scheme$score.function(p.values)
	#}
	n.exc <- rw.find.n.exc(score.scheme$score.scheme, n.seqs)
 w.max <- max(rw.walk2(scores))
 p.val <- rw.max.p(score.scheme$score.scheme, w.max, n.seqs, n.exc = n.exc)
 return(list(p.values=p.val,maxD=w.max,score.scheme=score.scheme$score.scheme[1]))
}
