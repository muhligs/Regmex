rw.transition.matrix <-
function(scoring.scheme, max.height) { #include
	max.step = length(scoring.scheme)-2
	tr.mat = matrix(0, nrow = max.height+2, ncol = max.height+2)
	rownames(tr.mat) = -1:max.height
	colnames(tr.mat) = -1:max.height
	# state -1 <= i <= max.height of the Markov chain
	# is entry no i+2 in the transition matrix
	for(i in 0:max.height) {
		# if you are at height i, possible _to_ states j are bounded by a ceiling = max.height.
		j = sapply(i+(-1:max.step), min, max.height)
		# k is last entry of j (possible to states) that is below max.height, added one (i.e. first entry of to states that reach max.height if max height is reachable. else one plus heighest state reachable.) 
		k = tail(which(j<max.height),1) + 1
		if(k < max.step+2) {
			# if more than one type of step
			# takes me above max.height
			# I need to sum up the corresponding probabilties
			# to transition to max.height
			tr.mat[i+2, j[1:(k-1)]+2] = scoring.scheme[1:(k-1)]#i+2 is the state index in the tr mat, i is the height of the walk. These are the only entries that get probabilities since others are unreachable.
			tr.mat[i+2, max.height+2] = sum(scoring.scheme[k:(max.step+2)])
		}
		else {
			tr.mat[i+2, j+2] = scoring.scheme
		}
	}
	# make -1 and max.height absorbing states
	tr.mat["-1", ] = c(1, rep(0, max.height+1))
	tr.mat[max.height+2, ] = c(rep(0, max.height+1), 1)
	return(tr.mat)
}
