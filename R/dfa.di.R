dfa.di <-
function(pm){ #include
	# get to know for each state how many incoming arrows 	
	c1<-0
	nt<-c()#previously observed nt
	dfa.state<-c()
	for( state in 1:dim(pm)[1]){#run through all rows (states) in FA
 	for(col.num in 1:dim(pm)[2]){#run through the bases
 		#if state is found in the current base col (i.e. if you can go to this state with this base (= incoming arrow))
 		if(rownames(pm)[state] %in% pm[,col.num]){
		  c1<-c1+1#index up the dfa.state vector
		  dfa.state[c1]<-rownames(pm)[state] #assign the state 
		  nt[c1]<-colnames(pm)[col.num]#assing the base
	  }
	}
	}
	# now dfa.state and nt holds all the states and base combos where we have incoming arrows
	states<-paste(dfa.state,nt,sep="") # the new "pseudo" states are defined
#	print(states)
	# make new dfa with the new states
	mat <- as.data.frame(matrix(0,ncol=4,nrow=length(states)))
	colnames(mat) <- c("A","C","G","T")
	rownames(mat) <- states
	c1 <- 0
for(ostate in dfa.state){
	c1 <- c1+1
	for(base in c("A","C","G","T")){
		mat[c1,base] <- paste(pm[ostate,base],base,sep="")
	}}
	return(mat)
}
