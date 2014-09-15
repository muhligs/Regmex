con.prob.di <-
function(di.nt.freq){ #include
# the function calculates a conditional probability matrix, row given col (row|col)
mat <- matrix(0,nrow=4,ncol=4)
colnames(mat) <- rownames(mat) <- c("A","C","G","T")
	sumvec <- c(sum(di.nt.freq[1:4]),sum(di.nt.freq[5:8]),sum(di.nt.freq[9:12]),sum(di.nt.freq[13:16]))
for(i in 1:4) mat[i,] <- di.nt.freq[(i-1)+c(1,5,9,13)]/sumvec
return(mat)
}
