prob.dist <-
function (pattern, seq, freq = FALSE, mode = FALSE, overlap = TRUE) #include
{
# this is the probabilities of moving from the initial state of the motif to other states given the sequence.
    prb.init.st <- lambda.con(pattern, seq, freq = freq, mode = mode, overlap = overlap)[pattern$startState,]
    st.st <- pattern$startState###
		end.st <- pattern$endState###
    nop <- n.obs.pat(pattern$pattern, seq$seq, overlap = overlap)
# 	print(prb.init.st)
if (mode == "rw"){nop <- nop + 1}
  nop2<-ifelse(nop %in% c(0,1),2,nop)#made to handle case nop = 0
    nSt <- dim(pattern$matrix)[1] # number of states
    prb.dst <- rep(0, nop2+1) # output probabilities for 0,1,...,nop observations
    for (i in 1:nop2) {
       ### prb.dst[i] <- sum(prb.init.st[(nSt * (i - 1) + 1):(nSt * i)])
    	if(i == 1){prb.dst[i] <- sum(prb.init.st[1:nSt][-end.st]);next}# probability for not having observed the motif. Sum of all dfa states (first embedding) except the end state.
    	prb.dst[i] <- sum(c(prb.init.st[(nSt * (i - 1) + 1):(nSt * i)][-end.st],prb.init.st[nSt * (i - 2)+end.st])) 
    	# e.g. for n'th observed motif, i.e. prb.dst[n], get prb.init.st from (n-1)*dim(single.dfa)+1 to n*dim(single.dfa) (i.e. the n'th embedding of the eTPM) except end states + end state of previous (n-1) embedding.
    }
		prb.dst[nop2+1] <- sum(prb.init.st[(nSt * (nop2-1)+end.st)]) # The final prb.dst corresponds to the number of observations. (+1 because [1] corresponds to 0 observations.)
if (mode == "rw"){nop <- nop - 1}
pnom <- ifelse(mode=="rw",sum(prb.dst[c(nop+1,nop+2)]),prb.dst[nop+1]) # probability of number of observed motifs.
    if (nop == 0) {
        return(list(prob.n.or.more = 1, n.obs.patterns = nop, 
            prob.dist = prb.dst, prob.1.or.more = 1 - prb.dst[1]))
    }
    if (nop == 1) {
        return(list(prob.n.or.more = 1 - prb.dst[1], n.obs.patterns = nop, 
            prob.dist = prb.dst, prob.1.or.more = 1 - prb.dst[1]))
    }
   return(list(prob.n.or.more = pnom, n.obs.patterns = nop, prob.dist = prb.dst, prob.1.or.more = 1 - prb.dst[1]))
		#return(list(prob.n.or.more = prb.dst[nop], n.obs.patterns = nop, prob.dist = prb.dst, prob.1.or.more = 1 - prb.dst[1]))
}
