prob.dist <-
function (pattern, seq, freq = FALSE, mode = FALSE, overlap = TRUE) #include
{
    if (freq[1]) {
#        print(freq)
        prb.init.st <- lambda.con(pattern, seq, freq = freq, mode = mode, overlap = overlap)[pattern$startState, 
            ]
    }
    else {
        prb.init.st <- lambda.con(pattern, seq, mode = mode, overlap = overlap)[pattern$startState, 
            ]
    }
    st.st <- pattern$startState###
		end.st <- pattern$endState###
    nop <- n.obs.pat(pattern$pattern, seq$seq, overlap = overlap)
# 	print(prb.init.st)
if (mode == "rw"){nop <- nop + 1}
    nop2 = nop
    if (nop == 0) {
        nop2 = 2
    }
    if (nop == 1) {
        nop2 = 2
    }

    nSt <- dim(pattern$matrix)[1]
		 ###prb.dst <- rep(0, nop2)
    prb.dst <- rep(0, nop2+1)###
    for (i in 1:nop2) {
       ### prb.dst[i] <- sum(prb.init.st[(nSt * (i - 1) + 1):(nSt * i)])
    	if(i == 1){prb.dst[i] <- sum(prb.init.st[1:nSt][-end.st]);next}###
    	prb.dst[i] <- sum(c(prb.init.st[(nSt * (i - 1) + 1):(nSt * i)][-end.st],prb.init.st[nSt * (i - 2)+end.st]))###
    }
		prb.dst[nop2+1] <- sum(prb.init.st[(nSt * (nop2-1)+end.st)])###
if (mode == "rw"){nop <- nop - 1}
pnom <- ifelse(mode=="rw",sum(prb.dst[c(nop+1,nop+2)]),prb.dst[nop+1])
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
